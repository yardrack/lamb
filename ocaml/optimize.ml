open Ir

type stats = {
  mutable folds : int;
  mutable branches : int;
  mutable deadbindings : int;
  mutable inlines : int;
  mutable betareductions : int;
}

type result = {
  before : int;
  after : int;
  expression : expression;
  stats : stats;
}

let empty_stats () =
  { folds = 0;
    branches = 0;
    deadbindings = 0;
    inlines = 0;
    betareductions = 0 }

let rec pure = function
  | Const _ | Var _ | Function _ | Constructor (_, None) -> true
  | Unary (_, argument) | Constructor (_, Some argument) -> pure argument
  | Primitive (operator, arguments) ->
      operator <> "/" && operator <> "mod" && List.for_all pure arguments
  | Tuplevalue items | Listvalue items -> List.for_all pure items
  | Consvalue (head, tail) -> pure head && pure tail
  | Let (_, value, body) -> pure value && pure body
  | Letrec _ | Apply _ | Switch _ -> false
  | If (condition, consequent, alternate) ->
      pure condition && pure consequent && pure alternate
  | Sequence (first, second) -> pure first && pure second

let rec occurrences name = function
  | Const _ | Constructor (_, None) -> 0
  | Var current -> if name = current then 1 else 0
  | Unary (_, argument) | Constructor (_, Some argument) -> occurrences name argument
  | Primitive (_, arguments) | Tuplevalue arguments | Listvalue arguments ->
      List.fold_left (fun count item -> count + occurrences name item) 0 arguments
  | Let (current, value, body) ->
      occurrences name value + if current = name then 0 else occurrences name body
  | Letrec (current, value, body) ->
      if current = name then 0
      else occurrences name value + occurrences name body
  | Function (_, parameter, body) ->
      if Names.mem name (pattern_binders parameter) then 0 else occurrences name body
  | Apply (callee, argument) | Consvalue (callee, argument)
  | Sequence (callee, argument) ->
      occurrences name callee + occurrences name argument
  | If (condition, consequent, alternate) ->
      occurrences name condition
      + occurrences name consequent
      + occurrences name alternate
  | Switch (target, cases) ->
      occurrences name target
      + List.fold_left
          (fun count (pattern, body) ->
            count
            + if Names.mem name (pattern_binders pattern) then 0
              else occurrences name body)
          0
          cases

let rec substitute name replacement expression =
  match expression with
  | Const _ | Constructor (_, None) -> expression
  | Var current -> if name = current then replacement else expression
  | Unary (operator, argument) ->
      Unary (operator, substitute name replacement argument)
  | Primitive (operator, arguments) ->
      Primitive (operator, List.map (substitute name replacement) arguments)
  | Let (current, value, body) ->
      let value = substitute name replacement value in
      if current = name then Let (current, value, body)
      else Let (current, value, substitute name replacement body)
  | Letrec (current, value, body) ->
      if current = name then expression
      else
        Letrec
          ( current,
            substitute name replacement value,
            substitute name replacement body )
  | Function (label, parameter, body) ->
      if Names.mem name (pattern_binders parameter) then expression
      else Function (label, parameter, substitute name replacement body)
  | Apply (callee, argument) ->
      Apply
        ( substitute name replacement callee,
          substitute name replacement argument )
  | If (condition, consequent, alternate) ->
      If
        ( substitute name replacement condition,
          substitute name replacement consequent,
          substitute name replacement alternate )
  | Switch (target, cases) ->
      Switch
        ( substitute name replacement target,
          List.map
            (fun (pattern, body) ->
              if Names.mem name (pattern_binders pattern) then pattern, body
              else pattern, substitute name replacement body)
            cases )
  | Tuplevalue items ->
      Tuplevalue (List.map (substitute name replacement) items)
  | Listvalue items ->
      Listvalue (List.map (substitute name replacement) items)
  | Consvalue (head, tail) ->
      Consvalue
        (substitute name replacement head, substitute name replacement tail)
  | Constructor (constructor, Some argument) ->
      Constructor (constructor, Some (substitute name replacement argument))
  | Sequence (first, second) ->
      Sequence
        (substitute name replacement first, substitute name replacement second)

let fold_unary operator value =
  match operator, value with
  | "-", Int value -> Some (Int (Int64.neg value))
  | _ -> None

let fold_binary operator left right =
  match operator, left, right with
  | "+", Int left, Int right -> Some (Int (Int64.add left right))
  | "-", Int left, Int right -> Some (Int (Int64.sub left right))
  | "*", Int left, Int right -> Some (Int (Int64.mul left right))
  | "/", Int _, Int 0L | "mod", Int _, Int 0L -> None
  | "/", Int left, Int right -> Some (Int (Int64.div left right))
  | "mod", Int left, Int right -> Some (Int (Int64.rem left right))
  | "^", Text left, Text right -> Some (Text (left ^ right))
  | "&&", Bool left, Bool right -> Some (Bool (left && right))
  | "||", Bool left, Bool right -> Some (Bool (left || right))
  | "=", left, right -> Some (Bool (constant_equal left right))
  | "<>", left, right -> Some (Bool (not (constant_equal left right)))
  | "<", Int left, Int right -> Some (Bool (left < right))
  | ">", Int left, Int right -> Some (Bool (left > right))
  | "<=", Int left, Int right -> Some (Bool (left <= right))
  | ">=", Int left, Int right -> Some (Bool (left >= right))
  | "<", Text left, Text right -> Some (Bool (left < right))
  | ">", Text left, Text right -> Some (Bool (left > right))
  | "<=", Text left, Text right -> Some (Bool (left <= right))
  | ">=", Text left, Text right -> Some (Bool (left >= right))
  | _ -> None

let pattern_matches pattern constant =
  match pattern with
  | Wildcard | Binder _ -> true
  | Constant expected -> constant_equal expected constant
  | Construct ("None", None) -> false
  | Nil | Cons _ | Tuple _ | Construct _ -> false

let optimize expression =
  let stats = empty_stats () in
  let rec descend expression =
    match expression with
    | Const _ | Var _ | Constructor (_, None) -> expression
    | Unary (operator, argument) ->
        let argument = descend argument in
        begin
          match argument with
          | Const value ->
              begin
                match fold_unary operator value with
                | Some value -> stats.folds <- stats.folds + 1; Const value
                | None -> Unary (operator, argument)
              end
          | _ -> Unary (operator, argument)
        end
    | Primitive (operator, arguments) ->
        let arguments = List.map descend arguments in
        begin
          match arguments with
          | [Const left; Const right] ->
              begin
                match fold_binary operator left right with
                | Some value -> stats.folds <- stats.folds + 1; Const value
                | None -> Primitive (operator, arguments)
              end
          | _ -> Primitive (operator, arguments)
        end
    | Let (name, value, body) ->
        let value = descend value in
        let body = descend body in
        let uses = occurrences name body in
        if uses = 0 && pure value then begin
          stats.deadbindings <- stats.deadbindings + 1;
          body
        end else if uses = 1 && pure value && size value <= 8 then begin
          stats.inlines <- stats.inlines + 1;
          descend (substitute name value body)
        end else Let (name, value, body)
    | Letrec (name, value, body) ->
        let value = descend value in
        let body = descend body in
        if occurrences name body = 0 && pure value then begin
          stats.deadbindings <- stats.deadbindings + 1;
          body
        end else Letrec (name, value, body)
    | Function (label, parameter, body) ->
        Function (label, parameter, descend body)
    | Apply (callee, argument) ->
        let callee = descend callee in
        let argument = descend argument in
        begin
          match callee with
          | Function (_, Binder parameter, body)
            when pure argument && size argument <= 8 ->
              stats.betareductions <- stats.betareductions + 1;
              descend (substitute parameter argument body)
          | _ -> Apply (callee, argument)
        end
    | If (condition, consequent, alternate) ->
        let condition = descend condition in
        begin
          match condition with
          | Const (Bool true) ->
              stats.branches <- stats.branches + 1;
              descend consequent
          | Const (Bool false) ->
              stats.branches <- stats.branches + 1;
              descend alternate
          | _ -> If (condition, descend consequent, descend alternate)
        end
    | Switch (target, cases) ->
        let target = descend target in
        let cases = List.map (fun (pattern, body) -> pattern, descend body) cases in
        begin
          match target with
          | Const value ->
              begin
                match List.find_opt (fun (pattern, _) -> pattern_matches pattern value) cases with
                | Some (_, body) -> stats.branches <- stats.branches + 1; body
                | None -> Switch (target, cases)
              end
          | _ -> Switch (target, cases)
        end
    | Tuplevalue items -> Tuplevalue (List.map descend items)
    | Listvalue items -> Listvalue (List.map descend items)
    | Consvalue (head, tail) -> Consvalue (descend head, descend tail)
    | Constructor (name, Some argument) -> Constructor (name, Some (descend argument))
    | Sequence (first, second) ->
        let first = descend first in
        let second = descend second in
        if pure first then begin
          stats.deadbindings <- stats.deadbindings + 1;
          second
        end else Sequence (first, second)
  in
  let before = size expression in
  let expression = descend expression in
  { before; after = size expression; expression; stats }

let format_stats result =
  String.concat
    "\n"
    [ Printf.sprintf "nodes       %d -> %d" result.before result.after;
      Printf.sprintf "folds       %d" result.stats.folds;
      Printf.sprintf "branches    %d" result.stats.branches;
      Printf.sprintf "dead lets   %d" result.stats.deadbindings;
      Printf.sprintf "inlines     %d" result.stats.inlines;
      Printf.sprintf "beta        %d" result.stats.betareductions ]
