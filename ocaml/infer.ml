open Syntax
open Typesystem

type annotation = {
  location : span;
  datatype : datatype;
}

type result = {
  root : expression;
  datatype : datatype;
  annotations : annotation list;
}

exception Error of string * span

type environment = (string * scheme) list

let integer = primitive "int"
let floating = primitive "float"
let boolean = primitive "bool"
let stringtype = primitive "string"
let character = primitive "char"
let unittype = primitive "unit"

let lookup environment name location =
  match List.assoc_opt name environment with
  | Some scheme -> scheme
  | None -> raise (Error ("unbound value " ^ name, location))

let replace environment name scheme =
  (name, scheme) :: List.remove_assoc name environment

let constructors level =
  let item = variable level in
  [ "None", { quantified =
                begin match item with Variable value -> [value] | _ -> [] end;
              body = option item };
    "Some", { quantified =
                begin match item with Variable value -> [value] | _ -> [] end;
              body = arrow item (option item) } ]

let with_type_error location functionvalue =
  try functionvalue () with
  | Typesystem.Error message -> raise (Error (message, location))

let rec bind_pattern environment names value expected level =
  let bind pattern expected =
    bind_pattern environment names pattern expected level
  in
  match value.pattern with
  | Pwildcard -> environment, names
  | Pvariable name ->
      if List.mem name names then
        raise
          (Error
             ("variable " ^ name ^ " is bound several times in this pattern",
              value.pspan));
      replace environment name (monomorphic expected), name :: names
  | Pliteral literal ->
      let datatype =
        match literal with
        | Integer _ -> integer
        | Float _ -> floating
        | Boolean _ -> boolean
        | String _ -> stringtype
        | Character _ -> character
        | Unit -> unittype
      in
      with_type_error value.pspan (fun () -> unify expected datatype);
      environment, names
  | Pnil ->
      with_type_error value.pspan (fun () -> unify expected (list (variable level)));
      environment, names
  | Pcons (head, tail) ->
      let item = variable level in
      with_type_error value.pspan (fun () -> unify expected (list item));
      let environment, names =
        bind_pattern environment names head item level
      in
      bind_pattern environment names tail (list item) level
  | Ptuple items ->
      let types = List.map (fun _ -> variable level) items in
      with_type_error value.pspan (fun () -> unify expected (tuple types));
      List.fold_left2
        (fun (environment, names) pattern datatype ->
          bind_pattern environment names pattern datatype level)
        (environment, names)
        items
        types
  | Pconstructor ("None", None) ->
      with_type_error value.pspan (fun () -> unify expected (option (variable level)));
      environment, names
  | Pconstructor ("None", Some _) ->
      raise (Error ("constructor None expects no argument", value.pspan))
  | Pconstructor ("Some", None) ->
      raise (Error ("constructor Some expects one argument", value.pspan))
  | Pconstructor ("Some", Some argument) ->
      let item = variable level in
      with_type_error value.pspan (fun () -> unify expected (option item));
      bind argument item
  | Pconstructor (name, _) ->
      raise (Error ("unknown constructor " ^ name, value.pspan))

let infer root =
  reset ();
  let annotations = ref [] in
  let constructorenvironment = constructors 0 in
  let annotate node datatype =
    let datatype = prune datatype in
    annotations := { location = node.span; datatype } :: !annotations;
    datatype
  in
  let rec descend environment level node =
    let inferred =
      match node.expression with
      | Literal literal ->
          begin
            match literal with
            | Integer _ -> integer
            | Float _ -> floating
            | Boolean _ -> boolean
            | String _ -> stringtype
            | Character _ -> character
            | Unit -> unittype
          end
      | Variable name ->
          instantiate level (lookup environment name node.span)
      | Constructor name ->
          begin
            match List.assoc_opt name constructorenvironment with
            | Some scheme -> instantiate level scheme
            | None -> raise (Error ("unknown constructor " ^ name, node.span))
          end
      | Unary ("-", argument) ->
          let argumenttype = descend environment level argument in
          with_type_error argument.span (fun () -> unify argumenttype integer);
          integer
      | Unary (operator, _) ->
          raise (Error ("unknown unary operator " ^ operator, node.span))
      | Binary (operator, left, right) ->
          let lefttype = descend environment level left in
          let righttype = descend environment level right in
          begin
            match operator with
            | "+" | "-" | "*" | "/" | "mod" ->
                with_type_error left.span (fun () -> unify lefttype integer);
                with_type_error right.span (fun () -> unify righttype integer);
                integer
            | "^" ->
                with_type_error left.span (fun () -> unify lefttype stringtype);
                with_type_error right.span (fun () -> unify righttype stringtype);
                stringtype
            | "&&" | "||" ->
                with_type_error left.span (fun () -> unify lefttype boolean);
                with_type_error right.span (fun () -> unify righttype boolean);
                boolean
            | "=" | "<>" ->
                with_type_error node.span (fun () -> unify lefttype righttype);
                boolean
            | "<" | ">" | "<=" | ">=" ->
                with_type_error node.span (fun () -> unify lefttype righttype);
                if not (is_ordered lefttype) then
                  raise
                    (Error
                       ( "operator " ^ operator ^ " does not order "
                         ^ format lefttype,
                         node.span ));
                boolean
            | _ -> raise (Error ("unknown operator " ^ operator, node.span))
          end
      | Cons (head, tail) ->
          let headtype = descend environment level head in
          let tailtype = descend environment level tail in
          with_type_error tail.span (fun () -> unify tailtype (list headtype));
          tailtype
      | List items ->
          let itemtype = variable level in
          List.iter
            (fun item ->
              let datatype = descend environment level item in
              with_type_error item.span (fun () -> unify datatype itemtype))
            items;
          list itemtype
      | Tuple items ->
          tuple (List.map (descend environment level) items)
      | Sequence (first, second) ->
          let firsttype = descend environment level first in
          with_type_error first.span (fun () -> unify firsttype unittype);
          descend environment level second
      | Conditional (condition, consequent, alternate) ->
          let conditiontype = descend environment level condition in
          let consequenttype = descend environment level consequent in
          let alternatetype = descend environment level alternate in
          with_type_error condition.span (fun () -> unify conditiontype boolean);
          with_type_error node.span (fun () -> unify consequenttype alternatetype);
          consequenttype
      | Function (parameter, body) ->
          let parametertype = variable (level + 1) in
          let nested, _ =
            bind_pattern environment [] parameter parametertype (level + 1)
          in
          let resulttype = descend nested (level + 1) body in
          arrow parametertype resulttype
      | Apply (callee, argument) ->
          let calleetype = descend environment level callee in
          let argumenttype = descend environment level argument in
          let resulttype = variable level in
          with_type_error callee.span (fun () ->
              unify calleetype (arrow argumenttype resulttype));
          resulttype
      | Let (binding, body) ->
          let valuetype, nestedenvironment =
            if binding.recursive then
              let provisional = variable (level + 1) in
              let recursiveenvironment =
                replace environment binding.name (monomorphic provisional)
              in
              let valuetype =
                descend recursiveenvironment (level + 1) binding.value
              in
              if not (equal provisional valuetype) then
                with_type_error binding.value.span (fun () ->
                    unify provisional valuetype);
              valuetype, recursiveenvironment
            else
              descend environment (level + 1) binding.value, environment
          in
          let nested =
            replace nestedenvironment binding.name (generalize level valuetype)
          in
          descend nested level body
      | Match (target, cases) ->
          let targettype = descend environment level target in
          let resulttype = variable level in
          List.iter
            (fun branch ->
              let nested, _ =
                bind_pattern environment [] branch.casepattern targettype level
              in
              let branchtype = descend nested level branch.casebody in
              with_type_error branch.casebody.span (fun () ->
                  unify resulttype branchtype))
            cases;
          resulttype
    in
    annotate node inferred
  in
  let datatype = descend [] 0 root in
  { root; datatype = prune datatype; annotations = List.rev !annotations }

let type_at (result : result) location =
  result.annotations
  |> List.find_opt (fun annotation -> annotation.location = location)
  |> Option.map (fun (annotation : annotation) -> annotation.datatype)

let format_result (result : result) =
  format result.datatype
