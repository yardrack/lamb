open Syntax
open Value

exception Error of string * span

let lookup environment name location =
  match List.assoc_opt name environment with
  | Some value -> !value
  | None -> raise (Error ("unbound runtime value " ^ name, location))

let literal = function
  | Integer value -> Vint value
  | Float value -> Vfloat value
  | Boolean value -> Vbool value
  | String value -> Vstring value
  | Character value -> Vchar value
  | Unit -> Vunit

let rec match_pattern value pattern bindings =
  match pattern.pattern, value with
  | Pwildcard, _ -> Some bindings
  | Pvariable name, value -> Some ((name, ref value) :: bindings)
  | Pliteral expected, value when Value.equal (literal expected) value ->
      Some bindings
  | Pnil, Vlist [] -> Some bindings
  | Pcons (head, tail), Vlist (first :: rest) ->
      begin
        match match_pattern first head bindings with
        | None -> None
        | Some bindings -> match_pattern (Vlist rest) tail bindings
      end
  | Ptuple patterns, Vtuple values
    when List.length patterns = List.length values ->
      List.fold_left2
        (fun result pattern value ->
          match result with
          | None -> None
          | Some bindings -> match_pattern value pattern bindings)
        (Some bindings)
        patterns
        values
  | Pconstructor (name, None), Vconstructor (actual, None)
    when name = actual -> Some bindings
  | Pconstructor (name, Some pattern), Vconstructor (actual, Some value)
    when name = actual -> match_pattern value pattern bindings
  | _ -> None

let integer location = function
  | Vint value -> value
  | _ -> raise (Error ("expected an integer", location))

let boolean location = function
  | Vbool value -> value
  | _ -> raise (Error ("expected a boolean", location))

let string location = function
  | Vstring value -> value
  | _ -> raise (Error ("expected a string", location))

let binary operator left right location =
  match operator with
  | "+" -> Vint (Int64.add (integer location left) (integer location right))
  | "-" -> Vint (Int64.sub (integer location left) (integer location right))
  | "*" -> Vint (Int64.mul (integer location left) (integer location right))
  | "/" ->
      let divisor = integer location right in
      if divisor = 0L then raise (Error ("division by zero", location));
      Vint (Int64.div (integer location left) divisor)
  | "mod" ->
      let divisor = integer location right in
      if divisor = 0L then raise (Error ("division by zero", location));
      Vint (Int64.rem (integer location left) divisor)
  | "^" -> Vstring (string location left ^ string location right)
  | "=" -> Vbool (Value.equal left right)
  | "<>" -> Vbool (not (Value.equal left right))
  | "<" -> Vbool (Value.compare left right < 0)
  | ">" -> Vbool (Value.compare left right > 0)
  | "<=" -> Vbool (Value.compare left right <= 0)
  | ">=" -> Vbool (Value.compare left right >= 0)
  | "&&" -> Vbool (boolean location left && boolean location right)
  | "||" -> Vbool (boolean location left || boolean location right)
  | _ -> raise (Error ("unknown operator " ^ operator, location))

let rec evaluate environment node =
  match node.expression with
  | Literal value -> literal value
  | Variable name -> lookup environment name node.span
  | Constructor "None" -> Vconstructor ("None", None)
  | Constructor name -> Vconstructorfunction name
  | Unary ("-", argument) ->
      Vint (Int64.neg (integer argument.span (evaluate environment argument)))
  | Unary (operator, _) ->
      raise (Error ("unknown unary operator " ^ operator, node.span))
  | Binary ("&&", left, right) ->
      let leftvalue = evaluate environment left in
      if boolean left.span leftvalue then evaluate environment right else Vbool false
  | Binary ("||", left, right) ->
      let leftvalue = evaluate environment left in
      if boolean left.span leftvalue then Vbool true else evaluate environment right
  | Binary (operator, left, right) ->
      binary
        operator
        (evaluate environment left)
        (evaluate environment right)
        node.span
  | Cons (head, tail) ->
      begin
        match evaluate environment tail with
        | Vlist items -> Vlist (evaluate environment head :: items)
        | _ -> raise (Error ("cons tail is not a list", tail.span))
      end
  | List items -> Vlist (List.map (evaluate environment) items)
  | Tuple items -> Vtuple (List.map (evaluate environment) items)
  | Sequence (first, second) ->
      ignore (evaluate environment first);
      evaluate environment second
  | Conditional (condition, consequent, alternate) ->
      if boolean condition.span (evaluate environment condition) then
        evaluate environment consequent
      else evaluate environment alternate
  | Function (parameter, body) -> Vclosure (parameter, body, environment)
  | Apply (callee, argument) ->
      let calleevalue = evaluate environment callee in
      let argumentvalue = evaluate environment argument in
      begin
        match calleevalue with
        | Vconstructorfunction name -> Vconstructor (name, Some argumentvalue)
        | Vclosure (parameter, body, closureenvironment) ->
            begin
              match match_pattern argumentvalue parameter [] with
              | Some bindings -> evaluate (bindings @ closureenvironment) body
              | None ->
                  raise
                    (Error
                       ( "function argument did not match its parameter pattern",
                         node.span ))
            end
        | _ -> raise (Error ("this expression is not a function", callee.span))
      end
  | Let (binding, body) ->
      if binding.recursive then
        let placeholder = ref Vunit in
        let nested = (binding.name, placeholder) :: environment in
        let value = evaluate nested binding.value in
        placeholder := value;
        evaluate nested body
      else
        let value = evaluate environment binding.value in
        evaluate ((binding.name, ref value) :: environment) body
  | Match (target, cases) ->
      let targetvalue = evaluate environment target in
      let rec choose = function
        | [] -> raise (Error ("pattern matching failed", node.span))
        | branch :: remaining ->
            begin
              match match_pattern targetvalue branch.casepattern [] with
              | Some bindings -> evaluate (bindings @ environment) branch.casebody
              | None -> choose remaining
            end
      in
      choose cases

let run expression =
  evaluate [] expression
