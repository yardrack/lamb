type span = {
  start : int;
  finish : int;
}

type literal =
  | Integer of int64
  | Float of float
  | Boolean of bool
  | String of string
  | Character of char
  | Unit

type pattern = {
  pattern : patternnode;
  pspan : span;
}

and patternnode =
  | Pwildcard
  | Pvariable of string
  | Pliteral of literal
  | Pnil
  | Pcons of pattern * pattern
  | Ptuple of pattern list
  | Pconstructor of string * pattern option

type expression = {
  expression : expressionnode;
  span : span;
}

and expressionnode =
  | Literal of literal
  | Variable of string
  | Constructor of string
  | Unary of string * expression
  | Binary of string * expression * expression
  | Cons of expression * expression
  | List of expression list
  | Tuple of expression list
  | Sequence of expression * expression
  | Conditional of expression * expression * expression
  | Function of pattern * expression
  | Apply of expression * expression
  | Let of binding * expression
  | Match of expression * case list

and binding = {
  name : string;
  value : expression;
  recursive : bool;
  bspan : span;
}

and case = {
  casepattern : pattern;
  casebody : expression;
  casespan : span;
}

let span start finish = { start; finish }

let expression expression start finish =
  { expression; span = span start finish }

let pattern pattern start finish =
  { pattern; pspan = span start finish }

let merge left right =
  { start = left.start; finish = right.finish }

let literal_name = function
  | Integer _ -> "int"
  | Float _ -> "float"
  | Boolean _ -> "bool"
  | String _ -> "string"
  | Character _ -> "char"
  | Unit -> "unit"

let string_of_literal = function
  | Integer value -> Int64.to_string value
  | Float value -> Printf.sprintf "%.12g" value
  | Boolean value -> string_of_bool value
  | String value -> Printf.sprintf "%S" value
  | Character value -> Printf.sprintf "'%c'" value
  | Unit -> "()"

let rec string_of_pattern value =
  match value.pattern with
  | Pwildcard -> "_"
  | Pvariable name -> name
  | Pliteral literal -> string_of_literal literal
  | Pnil -> "[]"
  | Pcons (head, tail) ->
      string_of_pattern head ^ " :: " ^ string_of_pattern tail
  | Ptuple items ->
      "(" ^ String.concat ", " (List.map string_of_pattern items) ^ ")"
  | Pconstructor (name, None) -> name
  | Pconstructor (name, Some argument) ->
      name ^ " " ^ string_of_pattern argument

let rec bound_names value =
  match value.pattern with
  | Pwildcard | Pliteral _ | Pnil -> []
  | Pvariable name -> [name]
  | Pcons (head, tail) -> bound_names head @ bound_names tail
  | Ptuple items -> List.concat_map bound_names items
  | Pconstructor (_, None) -> []
  | Pconstructor (_, Some argument) -> bound_names argument

let children value =
  match value.expression with
  | Literal _ | Variable _ | Constructor _ -> []
  | Unary (_, argument) -> [argument]
  | Binary (_, left, right) | Cons (left, right)
  | Sequence (left, right) | Apply (left, right) -> [left; right]
  | List items | Tuple items -> items
  | Conditional (condition, consequent, alternate) ->
      [condition; consequent; alternate]
  | Function (_, body) -> [body]
  | Let (binding, body) -> [binding.value; body]
  | Match (target, cases) ->
      target :: List.map (fun branch -> branch.casebody) cases

let rec fold functionvalue accumulator value =
  let accumulator = functionvalue accumulator value in
  List.fold_left (fold functionvalue) accumulator (children value)

let node_count value =
  fold (fun count _ -> count + 1) 0 value

let max_depth value =
  let rec descend depth current =
    List.fold_left
      (fun maximum child -> max maximum (descend (depth + 1) child))
      depth
      (children current)
  in
  descend 0 value

let rec map functionvalue value =
  let descend = map functionvalue in
  let expression =
    match value.expression with
    | Literal _ | Variable _ | Constructor _ as node -> node
    | Unary (operator, argument) -> Unary (operator, descend argument)
    | Binary (operator, left, right) ->
        Binary (operator, descend left, descend right)
    | Cons (head, tail) -> Cons (descend head, descend tail)
    | List items -> List (List.map descend items)
    | Tuple items -> Tuple (List.map descend items)
    | Sequence (first, second) -> Sequence (descend first, descend second)
    | Conditional (condition, consequent, alternate) ->
        Conditional (descend condition, descend consequent, descend alternate)
    | Function (parameter, body) -> Function (parameter, descend body)
    | Apply (callee, argument) -> Apply (descend callee, descend argument)
    | Let (binding, body) ->
        Let ({ binding with value = descend binding.value }, descend body)
    | Match (target, cases) ->
        Match
          ( descend target,
            List.map
              (fun branch -> { branch with casebody = descend branch.casebody })
              cases )
  in
  functionvalue { value with expression }
