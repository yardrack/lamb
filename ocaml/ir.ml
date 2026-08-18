type constant =
  | Int of int64
  | Float of float
  | Bool of bool
  | Text of string
  | Char of char
  | Unit

type pattern =
  | Wildcard
  | Binder of string
  | Constant of constant
  | Nil
  | Cons of pattern * pattern
  | Tuple of pattern list
  | Construct of string * pattern option

type expression =
  | Const of constant
  | Var of string
  | Unary of string * expression
  | Primitive of string * expression list
  | Let of string * expression * expression
  | Letrec of string * expression * expression
  | Function of string * pattern * expression
  | Apply of expression * expression
  | If of expression * expression * expression
  | Switch of expression * (pattern * expression) list
  | Tuplevalue of expression list
  | Listvalue of expression list
  | Consvalue of expression * expression
  | Constructor of string * expression option
  | Sequence of expression * expression

module Names = Set.Make (String)

type context = {
  mutable serial : int;
}

let fresh context hint =
  context.serial <- context.serial + 1;
  Printf.sprintf "%s/%d" hint context.serial

let constant = function
  | Syntax.Integer value -> Int value
  | Syntax.Float value -> Float value
  | Syntax.Boolean value -> Bool value
  | Syntax.String value -> Text value
  | Syntax.Character value -> Char value
  | Syntax.Unit -> Unit

let rec rename_pattern context environment value =
  match value.Syntax.pattern with
  | Syntax.Pwildcard -> Wildcard, environment
  | Syntax.Pvariable name ->
      let renamed = fresh context name in
      Binder renamed, (name, renamed) :: environment
  | Syntax.Pliteral value -> Constant (constant value), environment
  | Syntax.Pnil -> Nil, environment
  | Syntax.Pcons (head, tail) ->
      let head, environment = rename_pattern context environment head in
      let tail, environment = rename_pattern context environment tail in
      Cons (head, tail), environment
  | Syntax.Ptuple items ->
      let items, environment =
        List.fold_left
          (fun (items, environment) item ->
            let item, environment = rename_pattern context environment item in
            item :: items, environment)
          ([], environment)
          items
      in
      Tuple (List.rev items), environment
  | Syntax.Pconstructor (name, argument) ->
      begin
        match argument with
        | None -> Construct (name, None), environment
        | Some argument ->
            let argument, environment =
              rename_pattern context environment argument
            in
            Construct (name, Some argument), environment
      end

let lookup environment name =
  match List.assoc_opt name environment with
  | Some renamed -> renamed
  | None -> name

let lower root =
  let context = { serial = 0 } in
  let rec descend environment node =
    match node.Syntax.expression with
    | Syntax.Literal value -> Const (constant value)
    | Syntax.Variable name -> Var (lookup environment name)
    | Syntax.Constructor "None" -> Constructor ("None", None)
    | Syntax.Constructor name -> Constructor (name, None)
    | Syntax.Unary (operator, argument) ->
        Unary (operator, descend environment argument)
    | Syntax.Binary (operator, left, right) ->
        Primitive (operator, [descend environment left; descend environment right])
    | Syntax.Cons (head, tail) ->
        Consvalue (descend environment head, descend environment tail)
    | Syntax.List items -> Listvalue (List.map (descend environment) items)
    | Syntax.Tuple items -> Tuplevalue (List.map (descend environment) items)
    | Syntax.Sequence (first, second) ->
        Sequence (descend environment first, descend environment second)
    | Syntax.Conditional (condition, consequent, alternate) ->
        If
          ( descend environment condition,
            descend environment consequent,
            descend environment alternate )
    | Syntax.Function (parameter, body) ->
        let parameter, nested = rename_pattern context environment parameter in
        let name = fresh context "fun" in
        Function (name, parameter, descend nested body)
    | Syntax.Apply ({ Syntax.expression = Syntax.Constructor name; _ }, argument) ->
        Constructor (name, Some (descend environment argument))
    | Syntax.Apply (callee, argument) ->
        Apply (descend environment callee, descend environment argument)
    | Syntax.Let (binding, body) ->
        let renamed = fresh context binding.Syntax.name in
        let nested = (binding.Syntax.name, renamed) :: environment in
        if binding.Syntax.recursive then
          Letrec
            (renamed, descend nested binding.Syntax.value, descend nested body)
        else
          Let
            (renamed, descend environment binding.Syntax.value, descend nested body)
    | Syntax.Match (target, cases) ->
        let cases =
          List.map
            (fun branch ->
              let pattern, nested =
                rename_pattern context environment branch.Syntax.casepattern
              in
              pattern, descend nested branch.Syntax.casebody)
            cases
        in
        Switch (descend environment target, cases)
  in
  descend [] root

let constant_equal left right =
  match left, right with
  | Int left, Int right -> left = right
  | Float left, Float right -> left = right
  | Bool left, Bool right -> left = right
  | Text left, Text right -> left = right
  | Char left, Char right -> left = right
  | Unit, Unit -> true
  | _ -> false

let constant_text = function
  | Int value -> Int64.to_string value
  | Float value -> Printf.sprintf "%.12g" value
  | Bool value -> string_of_bool value
  | Text value -> Printf.sprintf "%S" value
  | Char value -> Printf.sprintf "'%c'" value
  | Unit -> "()"

let rec pattern_text = function
  | Wildcard -> "_"
  | Binder name -> name
  | Constant value -> constant_text value
  | Nil -> "[]"
  | Cons (head, tail) -> pattern_text head ^ " :: " ^ pattern_text tail
  | Tuple items -> "(" ^ String.concat ", " (List.map pattern_text items) ^ ")"
  | Construct (name, None) -> name
  | Construct (name, Some argument) -> name ^ " " ^ pattern_text argument

let rec free bound expression =
  let union values = List.fold_left Names.union Names.empty values in
  match expression with
  | Const _ | Constructor (_, None) -> Names.empty
  | Var name -> if Names.mem name bound then Names.empty else Names.singleton name
  | Unary (_, argument) -> free bound argument
  | Primitive (_, arguments) | Tuplevalue arguments | Listvalue arguments ->
      union (List.map (free bound) arguments)
  | Let (name, value, body) ->
      Names.union (free bound value) (free (Names.add name bound) body)
  | Letrec (name, value, body) ->
      let bound = Names.add name bound in
      Names.union (free bound value) (free bound body)
  | Function (_, parameter, body) ->
      free (Names.union bound (pattern_binders parameter)) body
  | Apply (callee, argument) | Consvalue (callee, argument)
  | Sequence (callee, argument) ->
      Names.union (free bound callee) (free bound argument)
  | If (condition, consequent, alternate) ->
      union [free bound condition; free bound consequent; free bound alternate]
  | Switch (target, cases) ->
      List.fold_left
        (fun names (pattern, body) ->
          Names.union names (free (Names.union bound (pattern_binders pattern)) body))
        (free bound target)
        cases
  | Constructor (_, Some argument) -> free bound argument

and pattern_binders = function
  | Wildcard | Constant _ | Nil | Construct (_, None) -> Names.empty
  | Binder name -> Names.singleton name
  | Cons (head, tail) -> Names.union (pattern_binders head) (pattern_binders tail)
  | Tuple items ->
      List.fold_left
        (fun names pattern -> Names.union names (pattern_binders pattern))
        Names.empty
        items
  | Construct (_, Some argument) -> pattern_binders argument

let free_variables expression =
  free Names.empty expression

let rec size = function
  | Const _ | Var _ | Constructor (_, None) -> 1
  | Unary (_, argument) | Constructor (_, Some argument) -> 1 + size argument
  | Primitive (_, arguments) | Tuplevalue arguments | Listvalue arguments ->
      1 + List.fold_left (fun total item -> total + size item) 0 arguments
  | Let (_, value, body) | Letrec (_, value, body)
  | Apply (value, body) | Consvalue (value, body) | Sequence (value, body) ->
      1 + size value + size body
  | Function (_, _, body) -> 1 + size body
  | If (condition, consequent, alternate) ->
      1 + size condition + size consequent + size alternate
  | Switch (target, cases) ->
      1 + size target
      + List.fold_left (fun total (_, body) -> total + size body) 0 cases

let rec format ?(depth = 0) expression =
  let padding = String.make (depth * 2) ' ' in
  let nested = format ~depth:(depth + 1) in
  let line text = padding ^ text in
  match expression with
  | Const value -> line ("const " ^ constant_text value)
  | Var name -> line ("var " ^ name)
  | Unary (operator, argument) -> line ("unary " ^ operator) ^ "\n" ^ nested argument
  | Primitive (operator, arguments) ->
      line ("primitive " ^ operator) ^ "\n"
      ^ String.concat "\n" (List.map nested arguments)
  | Let (name, value, body) ->
      line ("let " ^ name) ^ "\n" ^ nested value ^ "\n" ^ nested body
  | Letrec (name, value, body) ->
      line ("letrec " ^ name) ^ "\n" ^ nested value ^ "\n" ^ nested body
  | Function (name, parameter, body) ->
      line ("function " ^ name ^ " " ^ pattern_text parameter) ^ "\n" ^ nested body
  | Apply (callee, argument) ->
      line "apply" ^ "\n" ^ nested callee ^ "\n" ^ nested argument
  | If (condition, consequent, alternate) ->
      line "if" ^ "\n" ^ nested condition ^ "\n" ^ nested consequent ^ "\n" ^ nested alternate
  | Switch (target, cases) ->
      line "switch" ^ "\n" ^ nested target ^ "\n"
      ^ String.concat
          "\n"
          (List.map
             (fun (pattern, body) ->
               line ("case " ^ pattern_text pattern) ^ "\n" ^ nested body)
             cases)
  | Tuplevalue items ->
      line "tuple" ^ "\n" ^ String.concat "\n" (List.map nested items)
  | Listvalue items ->
      line "list" ^ "\n" ^ String.concat "\n" (List.map nested items)
  | Consvalue (head, tail) ->
      line "cons" ^ "\n" ^ nested head ^ "\n" ^ nested tail
  | Constructor (name, None) -> line ("constructor " ^ name)
  | Constructor (name, Some argument) ->
      line ("constructor " ^ name) ^ "\n" ^ nested argument
  | Sequence (first, second) ->
      line "sequence" ^ "\n" ^ nested first ^ "\n" ^ nested second
