open Syntax

let indent ?(depth = 1) text =
  let padding = String.make (depth * 2) ' ' in
  text
  |> String.split_on_char '\n'
  |> List.map (fun line -> padding ^ line)
  |> String.concat "\n"

let datatype inference node =
  match Infer.type_at inference node.span with
  | Some datatype -> Typesystem.format datatype
  | None -> "unknown"

let constant prefix = function
  | Integer value -> Printf.sprintf "%s_int %Ld" prefix value
  | Float value -> Printf.sprintf "%s_float %.12g" prefix value
  | Boolean value -> Printf.sprintf "%s_pointer %d" prefix (if value then 1 else 0)
  | String value -> Printf.sprintf "%s_string %S" prefix value
  | Character value -> Printf.sprintf "%s_char %C" prefix value
  | Unit -> prefix ^ "_pointer 0"

let primitive = function
  | "+" -> "Paddint"
  | "-" -> "Psubint"
  | "*" -> "Pmulint"
  | "/" -> "Pdivint"
  | "mod" -> "Pmodint"
  | "^" -> "Pstringconcat"
  | "=" -> "Pcompare Ceq"
  | "<>" -> "Pcompare Cne"
  | "<" -> "Pcompare Clt"
  | ">" -> "Pcompare Cgt"
  | "<=" -> "Pcompare Cle"
  | ">=" -> "Pcompare Cge"
  | "&&" -> "Psequand"
  | "||" -> "Psequor"
  | operator -> "Punknown_" ^ operator

let rec typedtree inference node =
  let datatype = datatype inference node in
  match node.expression with
  | Literal literal ->
      Printf.sprintf "Texp_constant %s : %s" (constant "Const" literal) datatype
  | Variable name -> Printf.sprintf "Texp_ident %s : %s" name datatype
  | Constructor name -> Printf.sprintf "Texp_construct %s : %s" name datatype
  | Unary (operator, argument) ->
      Printf.sprintf
        "Texp_apply (~%s) : %s\n%s"
        operator
        datatype
        (indent (typedtree inference argument))
  | Binary (operator, left, right) ->
      Printf.sprintf
        "Texp_apply (%s) : %s\n%s\n%s"
        operator
        datatype
        (indent (typedtree inference left))
        (indent (typedtree inference right))
  | Cons (head, tail) ->
      Printf.sprintf
        "Texp_construct (::) : %s\n%s\n%s"
        datatype
        (indent (typedtree inference head))
        (indent (typedtree inference tail))
  | List items ->
      let items =
        List.map (fun item -> indent (typedtree inference item)) items
        |> String.concat "\n"
      in
      Printf.sprintf "Texp_list : %s\n%s" datatype items
  | Tuple items ->
      let items =
        List.map (fun item -> indent (typedtree inference item)) items
        |> String.concat "\n"
      in
      Printf.sprintf "Texp_tuple : %s\n%s" datatype items
  | Sequence (first, second) ->
      Printf.sprintf
        "Texp_sequence : %s\n%s\n%s"
        datatype
        (indent (typedtree inference first))
        (indent (typedtree inference second))
  | Conditional (condition, consequent, alternate) ->
      Printf.sprintf
        "Texp_ifthenelse : %s\n%s\n%s\n%s"
        datatype
        (indent (typedtree inference condition))
        (indent (typedtree inference consequent))
        (indent (typedtree inference alternate))
  | Function (parameter, body) ->
      Printf.sprintf
        "Texp_function %s : %s\n%s"
        (string_of_pattern parameter)
        datatype
        (indent (typedtree inference body))
  | Apply (callee, argument) ->
      Printf.sprintf
        "Texp_apply : %s\n%s\n%s"
        datatype
        (indent (typedtree inference callee))
        (indent (typedtree inference argument))
  | Let (binding, body) ->
      Printf.sprintf
        "Texp_let %s %s : %s\n%s\n%s"
        (if binding.recursive then "Recursive" else "Nonrecursive")
        binding.name
        datatype
        (indent (typedtree inference binding.value))
        (indent (typedtree inference body))
  | Match (target, cases) ->
      let cases =
        cases
        |> List.map (fun branch ->
               Printf.sprintf
                 "Tcase %s\n%s"
                 (string_of_pattern branch.casepattern)
                 (indent (typedtree inference branch.casebody)))
        |> List.map indent
        |> String.concat "\n"
      in
      Printf.sprintf
        "Texp_match : %s\n%s\n%s"
        datatype
        (indent (typedtree inference target))
        cases

let rec functional family node =
  let prefix = if family = `Lambda then "L" else "U" in
  let descend = functional family in
  match node.expression with
  | Literal literal ->
      Printf.sprintf
        "(%sconst (%s))"
        prefix
        (constant (if family = `Lambda then "Const" else "Uconst") literal)
  | Variable name -> Printf.sprintf "(%svar %s)" prefix name
  | Constructor name -> Printf.sprintf "(%sconstruct %s)" prefix name
  | Unary (_, argument) ->
      Printf.sprintf "(%sprim Pnegint\n%s)" prefix (indent (descend argument))
  | Binary (operator, left, right) ->
      Printf.sprintf
        "(%sprim %s\n%s\n%s)"
        prefix
        (primitive operator)
        (indent (descend left))
        (indent (descend right))
  | Cons (head, tail) ->
      Printf.sprintf
        "(%sprim Pmakeblock Cons\n%s\n%s)"
        prefix
        (indent (descend head))
        (indent (descend tail))
  | List items ->
      List.fold_right
        (fun item tail ->
          Printf.sprintf
            "(%sprim Pmakeblock Cons\n%s\n%s)"
            prefix
            (indent (descend item))
            (indent tail))
        items
        (Printf.sprintf "(%sconst Emptylist)" prefix)
  | Tuple items ->
      let items =
        items |> List.map (fun item -> indent (descend item)) |> String.concat "\n"
      in
      Printf.sprintf "(%sprim Pmakeblock Tuple\n%s)" prefix items
  | Sequence (first, second) ->
      Printf.sprintf
        "(%ssequence\n%s\n%s)"
        prefix
        (indent (descend first))
        (indent (descend second))
  | Conditional (condition, consequent, alternate) ->
      Printf.sprintf
        "(%sifthenelse\n%s\n%s\n%s)"
        prefix
        (indent (descend condition))
        (indent (descend consequent))
        (indent (descend alternate))
  | Function (parameter, body) ->
      if family = `Lambda then
        Printf.sprintf
          "(Lfunction %s\n%s)"
          (string_of_pattern parameter)
          (indent (descend body))
      else
        let bound =
          bound_names parameter
          |> List.fold_left
               (fun names name -> Analysis.Names.add name names)
               Analysis.Names.empty
        in
        let captured =
          Analysis.free_variables bound Analysis.Names.empty body
          |> Analysis.Names.elements
        in
        Printf.sprintf
          "(Uclosure\n%s\n%s\n%s)"
          (indent
             (Printf.sprintf
                "function %s arity 1"
                (string_of_pattern parameter)))
          (indent
             (Printf.sprintf
                "environment [%s]"
                (if captured = [] then "empty" else String.concat ", " captured)))
          (indent (descend body))
  | Apply (callee, argument) ->
      Printf.sprintf
        "(%s\n%s\n%s)"
        (if family = `Lambda then "Lapply" else "Ugenericapply")
        (indent (descend callee))
        (indent (descend argument))
  | Let (binding, body) ->
      Printf.sprintf
        "(%s%s %s\n%s\n%s)"
        prefix
        (if binding.recursive then "letrec" else "let")
        binding.name
        (indent (descend binding.value))
        (indent (descend body))
  | Match (target, cases) ->
      let cases =
        cases
        |> List.map (fun branch ->
               Printf.sprintf
                 "case %s\n%s"
                 (string_of_pattern branch.casepattern)
                 (indent (descend branch.casebody)))
        |> List.map indent
        |> String.concat "\n"
      in
      Printf.sprintf
        "(%sswitch\n%s\n%s)"
        prefix
        (indent (descend target))
        cases

let lambda node = functional `Lambda node
let clambda node = functional `Clambda node

type cmmstate = {
  mutable serial : int;
  mutable functions : string list;
}

let next state =
  state.serial <- state.serial + 1;
  Printf.sprintf "v%d" state.serial

let emit lines line =
  lines := line :: !lines

let rec cmmnode state lines node =
  match node.expression with
  | Literal (Integer value) ->
      let name = next state in
      emit lines (Printf.sprintf "%s = %LdL" name (Value.encode_integer value));
      name
  | Literal (Boolean value) ->
      let name = next state in
      emit lines (Printf.sprintf "%s = %dL" name (if value then 3 else 1));
      name
  | Literal Unit ->
      let name = next state in
      emit lines (name ^ " = 1L");
      name
  | Literal (Float value) ->
      let name = next state in
      emit lines (Printf.sprintf "%s = alloc_float(%.12g)" name value);
      name
  | Literal (String value) ->
      let name = next state in
      emit lines (Printf.sprintf "%s = alloc_string(%S)" name value);
      name
  | Literal (Character value) ->
      let name = next state in
      emit
        lines
        (Printf.sprintf
           "%s = %LdL"
           name
           (Value.encode_integer (Int64.of_int (Char.code value))));
      name
  | Variable name -> name
  | Constructor name ->
      let result = next state in
      emit lines (Printf.sprintf "%s = constructor_%s" result name);
      result
  | Unary (_, argument) ->
      let argument = cmmnode state lines argument in
      let result = next state in
      emit lines (Printf.sprintf "%s = 2L - %s" result argument);
      result
  | Binary (operator, left, right) ->
      let left = cmmnode state lines left in
      let right = cmmnode state lines right in
      let result = next state in
      let operation =
        match operator with
        | "+" -> Printf.sprintf "%s + %s - 1L" left right
        | "-" -> Printf.sprintf "%s - %s + 1L" left right
        | "*" -> Printf.sprintf "tag_int(untag_int(%s) * untag_int(%s))" left right
        | "/" -> Printf.sprintf "tag_int(untag_int(%s) / untag_int(%s))" left right
        | "mod" -> Printf.sprintf "tag_int(untag_int(%s) %% untag_int(%s))" left right
        | "^" -> Printf.sprintf "caml_string_concat(%s, %s)" left right
        | "=" -> Printf.sprintf "tag_bool(%s == %s)" left right
        | "<>" -> Printf.sprintf "tag_bool(%s != %s)" left right
        | "<" -> Printf.sprintf "tag_bool(%s < %s)" left right
        | ">" -> Printf.sprintf "tag_bool(%s > %s)" left right
        | "<=" -> Printf.sprintf "tag_bool(%s <= %s)" left right
        | ">=" -> Printf.sprintf "tag_bool(%s >= %s)" left right
        | "&&" -> Printf.sprintf "branch_and(%s, %s)" left right
        | "||" -> Printf.sprintf "branch_or(%s, %s)" left right
        | unknown -> Printf.sprintf "unknown_%s(%s, %s)" unknown left right
      in
      emit lines (Printf.sprintf "%s = %s" result operation);
      result
  | Cons (head, tail) ->
      let head = cmmnode state lines head in
      let tail = cmmnode state lines tail in
      let result = next state in
      emit lines (Printf.sprintf "%s = alloc_cons(%s, %s)" result head tail);
      result
  | List items ->
      List.fold_right
        (fun item tail ->
          let item = cmmnode state lines item in
          let result = next state in
          emit lines (Printf.sprintf "%s = alloc_cons(%s, %s)" result item tail);
          result)
        items
        "1L"
  | Tuple items ->
      let fields = List.map (cmmnode state lines) items in
      let result = next state in
      emit
        lines
        (Printf.sprintf "%s = alloc_tuple(%s)" result (String.concat ", " fields));
      result
  | Sequence (first, second) ->
      ignore (cmmnode state lines first);
      cmmnode state lines second
  | Conditional (condition, consequent, alternate) ->
      let condition = cmmnode state lines condition in
      let consequentlines = ref [] in
      let consequent = cmmnode state consequentlines consequent in
      let alternatelines = ref [] in
      let alternate = cmmnode state alternatelines alternate in
      let result = next state in
      emit lines (Printf.sprintf "%s = if %s != 1L then {" result condition);
      List.iter (fun line -> emit lines ("  " ^ line)) (List.rev !consequentlines);
      emit lines ("  return " ^ consequent);
      emit lines "} else {";
      List.iter (fun line -> emit lines ("  " ^ line)) (List.rev !alternatelines);
      emit lines ("  return " ^ alternate);
      emit lines "}";
      result
  | Function (parameter, body) ->
      let result = next state in
      let bodylines = ref [] in
      let bodyresult = cmmnode state bodylines body in
      let definition =
        Printf.sprintf
          "function code_%s(%s, env) {\n%s\n  return %s\n}"
          result
          (string_of_pattern parameter)
          (indent (String.concat "\n" (List.rev !bodylines)))
          bodyresult
      in
      state.functions <- definition :: state.functions;
      emit lines (Printf.sprintf "%s = alloc_closure(code_%s)" result result);
      result
  | Apply (callee, argument) ->
      let callee = cmmnode state lines callee in
      let argument = cmmnode state lines argument in
      let result = next state in
      emit
        lines
        (Printf.sprintf "%s = call_closure(%s, %s)" result callee argument);
      result
  | Let (binding, body) ->
      let value = cmmnode state lines binding.value in
      emit lines (Printf.sprintf "%s = %s" binding.name value);
      cmmnode state lines body
  | Match (target, cases) ->
      let target = cmmnode state lines target in
      let branches =
        List.map
          (fun branch ->
            let branchlines = ref [] in
            let result = cmmnode state branchlines branch.casebody in
            Printf.sprintf
              "case %s {\n%s\n  return %s\n}"
              (string_of_pattern branch.casepattern)
              (indent (String.concat "\n" (List.rev !branchlines)))
              result)
          cases
      in
      let result = next state in
      emit
        lines
        (Printf.sprintf
           "%s = switch %s {\n%s\n}"
           result
           target
           (indent (String.concat "\n" branches)));
      result

let cmm node =
  let state = { serial = 0; functions = [] } in
  let lines = ref [] in
  let result = cmmnode state lines node in
  String.concat
    "\n"
    ([ "function camlLamb__entry() {";
       indent (String.concat "\n" (List.rev !lines));
       "  return " ^ result;
       "}" ]
     @ List.rev state.functions)

let assembly node =
  let constants, operators =
    fold
      (fun (constants, operators) node ->
        match node.expression with
        | Literal (Integer value) -> value :: constants, operators
        | Binary (operator, _, _) -> constants, operator :: operators
        | _ -> constants, operators)
      ([], [])
      node
  in
  let constants = List.rev constants in
  let operators = List.rev operators in
  let lines = ref ["  mov rbp, rsp"; "  push rbp"; "camlLamb__entry:"] in
  List.iteri
    (fun index value ->
      let register = if index mod 2 = 0 then "rax" else "rbx" in
      lines :=
        Printf.sprintf "  mov %s, %Ld" register (Value.encode_integer value)
        :: !lines)
    constants;
  List.iter
    (fun operator ->
      let instructions =
        match operator with
        | "+" -> ["  lea rax, [rax + rbx - 1]"]
        | "-" -> ["  sub rax, rbx"; "  add rax, 1"]
        | "*" ->
            ["  sar rax, 1"; "  sar rbx, 1"; "  imul rax, rbx";
             "  lea rax, [rax * 2 + 1]"]
        | "/" | "mod" ->
            ["  sar rax, 1"; "  sar rbx, 1"; "  cqo"; "  idiv rbx";
             "  lea rax, [rax * 2 + 1]"]
        | operator -> ["  call camlLamb__" ^ primitive operator]
      in
      List.iter (fun instruction -> lines := instruction :: !lines) instructions)
    operators;
  if constants = [] then lines := "  mov rax, 1" :: !lines;
  lines := "  ret" :: "  pop rbp" :: !lines;
  String.concat "\n" (List.rev !lines)
