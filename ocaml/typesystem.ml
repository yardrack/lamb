type variable = {
  id : int;
  mutable level : int;
  mutable link : datatype option;
}

and datatype =
  | Primitive of string
  | Variable of variable
  | Arrow of datatype * datatype
  | Tuple of datatype list
  | List of datatype
  | Option of datatype

type scheme = {
  quantified : variable list;
  body : datatype;
}

exception Error of string

let serial = ref 0

let reset () =
  serial := 0

let primitive name = Primitive name

let variable level =
  let value = { id = !serial; level; link = None } in
  incr serial;
  Variable value

let arrow parameter result = Arrow (parameter, result)
let tuple items = Tuple items
let list item = List item
let option item = Option item

let rec prune = function
  | Variable ({ link = Some target; _ } as value) ->
      let target = prune target in
      value.link <- Some target;
      target
  | value -> value

let children datatype =
  match prune datatype with
  | Arrow (parameter, result) -> [parameter; result]
  | Tuple items -> items
  | List item | Option item -> [item]
  | Primitive _ | Variable _ -> []

let rec occurs target datatype =
  match prune datatype with
  | Variable value when value == target -> true
  | datatype -> List.exists (occurs target) (children datatype)

let rec lower level datatype =
  match prune datatype with
  | Variable value -> value.level <- min value.level level
  | datatype -> List.iter (lower level) (children datatype)

let rec unify left right =
  let left = prune left in
  let right = prune right in
  if left == right then ()
  else
    match left, right with
    | Variable variable, datatype ->
        if occurs variable datatype then
          raise (Error "this expression creates an infinite type");
        lower variable.level datatype;
        variable.link <- Some datatype
    | datatype, Variable variable -> unify (Variable variable) datatype
    | Primitive left, Primitive right when left = right -> ()
    | Arrow (leftparameter, leftresult), Arrow (rightparameter, rightresult) ->
        unify leftparameter rightparameter;
        unify leftresult rightresult
    | List left, List right | Option left, Option right -> unify left right
    | Tuple left, Tuple right when List.length left = List.length right ->
        List.iter2 unify left right
    | _ -> raise (Error "these types are incompatible")

let rec collect level found datatype =
  match prune datatype with
  | Variable variable
    when variable.link = None && variable.level > level ->
      if List.exists (fun entry -> entry == variable) found then found
      else variable :: found
  | datatype -> List.fold_left (collect level) found (children datatype)

let generalize level datatype =
  { quantified = collect level [] datatype; body = datatype }

let monomorphic datatype =
  { quantified = []; body = datatype }

let instantiate level scheme =
  let replacements =
    List.map
      (fun quantified ->
        match variable level with
        | Variable fresh -> (quantified, Variable fresh)
        | _ -> assert false)
      scheme.quantified
  in
  let rec copy datatype =
    match prune datatype with
    | Variable variable ->
        begin
          match List.find_opt (fun (source, _) -> source == variable) replacements with
          | Some (_, target) -> target
          | None -> Variable variable
        end
    | Primitive name -> Primitive name
    | Arrow (parameter, result) -> Arrow (copy parameter, copy result)
    | Tuple items -> Tuple (List.map copy items)
    | List item -> List (copy item)
    | Option item -> Option (copy item)
  in
  copy scheme.body

let variable_name index =
  let letter = Char.chr (Char.code 'a' + (index mod 26)) in
  if index < 26 then Printf.sprintf "'%c" letter
  else Printf.sprintf "'%c%d" letter (index / 26)

let format datatype =
  let names = Hashtbl.create 16 in
  let rec render datatype =
    match prune datatype with
    | Primitive name -> name
    | Variable variable ->
        begin
          match Hashtbl.find_opt names variable.id with
          | Some name -> name
          | None ->
              let name = variable_name (Hashtbl.length names) in
              Hashtbl.add names variable.id name;
              name
        end
    | Arrow (parameter, result) ->
        let parametertext = render parameter in
        let parametertext =
          match prune parameter with
          | Arrow _ -> "(" ^ parametertext ^ ")"
          | _ -> parametertext
        in
        parametertext ^ " -> " ^ render result
    | Tuple items -> String.concat " * " (List.map render items)
    | List item ->
        let text = render item in
        begin
          match prune item with
          | Arrow _ | Tuple _ -> "(" ^ text ^ ") list"
          | _ -> text ^ " list"
        end
    | Option item ->
        let text = render item in
        begin
          match prune item with
          | Arrow _ | Tuple _ -> "(" ^ text ^ ") option"
          | _ -> text ^ " option"
        end
  in
  render datatype

let equal left right =
  let rec compare left right =
    match prune left, prune right with
    | Primitive left, Primitive right -> left = right
    | Variable left, Variable right -> left == right
    | Arrow (lp, lr), Arrow (rp, rr) -> compare lp rp && compare lr rr
    | Tuple left, Tuple right ->
        List.length left = List.length right && List.for_all2 compare left right
    | List left, List right | Option left, Option right -> compare left right
    | _ -> false
  in
  compare left right

let is_numeric datatype =
  match prune datatype with
  | Primitive "int" | Primitive "float" -> true
  | _ -> false

let is_ordered datatype =
  match prune datatype with
  | Primitive ("int" | "float" | "string" | "char") -> true
  | _ -> false
