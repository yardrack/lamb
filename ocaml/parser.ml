open Syntax
open Token

type state = {
  source : string;
  tokens : token array;
  mutable index : int;
}

let current state =
  state.tokens.(state.index)

let advance state =
  let token = current state in
  state.index <- state.index + 1;
  token

let take state text =
  if (current state).text = text then Some (advance state) else None

let expect state text =
  match take state text with
  | Some token -> token
  | None ->
      let token = current state in
      let found = if token.kind = Eof then "end of input" else token.text in
      raise
        (Error
           ( Printf.sprintf "expected %S but found %S" text found,
             token.start,
             token.finish ))

let name state =
  let token = current state in
  if token.kind <> Identifier then
    raise (Error ("expected an identifier", token.start, token.finish));
  ignore (advance state);
  token

let precedence = function
  | ";" -> Some (5, `Right)
  | "||" -> Some (10, `Left)
  | "&&" -> Some (20, `Left)
  | "=" | "<>" | "<" | ">" | "<=" | ">=" -> Some (30, `Left)
  | "::" -> Some (35, `Right)
  | "+" | "-" -> Some (40, `Left)
  | "^" -> Some (40, `Right)
  | "*" | "/" | "mod" -> Some (50, `Left)
  | _ -> None

let starts_pattern token =
  match token.kind with
  | Identifier | Integer | String | Character | Constructor -> true
  | Keyword when token.text = "true" || token.text = "false" -> true
  | Punctuation when token.text = "(" || token.text = "[" -> true
  | _ -> false

let starts_atom token =
  match token.kind with
  | Identifier | Constructor | Integer | Float | String | Character -> true
  | Keyword
    when token.text = "true" || token.text = "false" || token.text = "begin" ->
      true
  | Punctuation when token.text = "(" || token.text = "[" -> true
  | _ -> false

let literal token =
  match token.kind with
  | Token.Integer -> Syntax.Integer (Int64.of_string token.text)
  | Token.Float -> Syntax.Float (float_of_string token.text)
  | Token.String -> Syntax.String token.text
  | Token.Character -> Syntax.Character token.text.[0]
  | Token.Keyword when token.text = "true" -> Syntax.Boolean true
  | Token.Keyword when token.text = "false" -> Syntax.Boolean false
  | _ -> raise (Error ("expected a literal", token.start, token.finish))

let rec parse_pattern state =
  let left = pattern_atom state in
  match take state "::" with
  | None -> left
  | Some _ ->
      let tail = parse_pattern state in
      pattern (Pcons (left, tail)) left.pspan.start tail.pspan.finish

and pattern_atom state =
  let token = current state in
  match token.kind with
  | Identifier ->
      ignore (advance state);
      if token.text = "_" then pattern Pwildcard token.start token.finish
      else pattern (Pvariable token.text) token.start token.finish
  | Integer | String | Character ->
      ignore (advance state);
      pattern (Pliteral (literal token)) token.start token.finish
  | Keyword when token.text = "true" || token.text = "false" ->
      ignore (advance state);
      pattern (Pliteral (literal token)) token.start token.finish
  | Constructor ->
      ignore (advance state);
      let argument =
        if starts_pattern (current state) then Some (pattern_atom state) else None
      in
      let finish =
        match argument with Some value -> value.pspan.finish | None -> token.finish
      in
      pattern (Pconstructor (token.text, argument)) token.start finish
  | Punctuation when token.text = "[" ->
      ignore (advance state);
      let close = expect state "]" in
      pattern Pnil token.start close.finish
  | Punctuation when token.text = "(" ->
      ignore (advance state);
      begin
        match take state ")" with
        | Some close -> pattern (Pliteral Unit) token.start close.finish
        | None ->
            let first = parse_pattern state in
            begin
              match take state "," with
              | None ->
                  let close = expect state ")" in
                  { first with pspan = span token.start close.finish }
              | Some _ ->
                  let rec items values =
                    let value = parse_pattern state in
                    match take state "," with
                    | Some _ -> items (value :: values)
                    | None -> List.rev (value :: values)
                  in
                  let values = items [first] in
                  let close = expect state ")" in
                  pattern (Ptuple values) token.start close.finish
            end
      end
  | _ ->
      let found = if token.kind = Eof then "end of input" else token.text in
      raise
        (Error
           ( Printf.sprintf "expected a pattern but found %S" found,
             token.start,
             token.finish ))

let rec parse_expression state minimum =
  let rec continue left =
    let token = current state in
    if starts_atom token && 80 >= minimum then
      let argument = parse_expression state 81 in
      continue
        (expression
           (Apply (left, argument))
           left.span.start
           argument.span.finish)
    else
      match precedence token.text with
      | Some (priority, associativity) when priority >= minimum ->
          ignore (advance state);
          let next = if associativity = `Right then priority else priority + 1 in
          let right = parse_expression state next in
          let node =
            if token.text = ";" then Sequence (left, right)
            else if token.text = "::" then Cons (left, right)
            else Binary (token.text, left, right)
          in
          continue (expression node left.span.start right.span.finish)
      | _ -> left
  in
  continue (prefix state)

and prefix state =
  let token = current state in
  match token.kind, token.text with
  | Keyword, "let" ->
      ignore (advance state);
      let_expression state token.start
  | Keyword, "fun" ->
      ignore (advance state);
      function_expression state token.start
  | Keyword, "function" ->
      ignore (advance state);
      function_cases state token.start
  | Keyword, "if" ->
      ignore (advance state);
      conditional state token.start
  | Keyword, "match" ->
      ignore (advance state);
      match_expression state token.start
  | Keyword, "begin" ->
      ignore (advance state);
      let value = parse_expression state 0 in
      let close = expect state "end" in
      { value with span = span token.start close.finish }
  | Operator, "-" ->
      ignore (advance state);
      let argument = parse_expression state 70 in
      expression (Unary ("-", argument)) token.start argument.span.finish
  | (Integer | Float | String | Character), _ ->
      ignore (advance state);
      expression (Literal (literal token)) token.start token.finish
  | Keyword, ("true" | "false") ->
      ignore (advance state);
      expression (Literal (literal token)) token.start token.finish
  | Identifier, _ ->
      ignore (advance state);
      expression (Variable token.text) token.start token.finish
  | Constructor, _ ->
      ignore (advance state);
      expression (Constructor token.text) token.start token.finish
  | Punctuation, "[" ->
      ignore (advance state);
      list_expression state token.start
  | Punctuation, "(" ->
      ignore (advance state);
      parenthesized state token.start
  | _ ->
      let found = if token.kind = Eof then "end of input" else token.text in
      raise
        (Error
           ( Printf.sprintf "expected an expression but found %S" found,
             token.start,
             token.finish ))

and parenthesized state start =
  match take state ")" with
  | Some close -> expression (Literal Unit) start close.finish
  | None ->
      let first = parse_expression state 0 in
      begin
        match take state "," with
        | None ->
            let close = expect state ")" in
            { first with span = span start close.finish }
        | Some _ ->
            let rec items values =
              let value = parse_expression state 6 in
              match take state "," with
              | Some _ -> items (value :: values)
              | None -> List.rev (value :: values)
            in
            let values = items [first] in
            let close = expect state ")" in
            expression (Tuple values) start close.finish
      end

and list_expression state start =
  match take state "]" with
  | Some close -> expression (List []) start close.finish
  | None ->
      let first = parse_expression state 6 in
      let rec items values =
        match take state ";" with
        | Some _ -> items (parse_expression state 6 :: values)
        | None -> List.rev values
      in
      let values = items [first] in
      let close = expect state "]" in
      expression (List values) start close.finish

and let_expression state start =
  let recursive = Option.is_some (take state "rec") in
  let identifier = name state in
  let rec parameters values =
    if starts_pattern (current state) then
      parameters (parse_pattern state :: values)
    else List.rev values
  in
  let parameters = parameters [] in
  ignore (expect state "=");
  let raw = parse_expression state 0 in
  let value =
    List.fold_right
      (fun parameter body ->
        expression
          (Function (parameter, body))
          parameter.pspan.start
          body.span.finish)
      parameters raw
  in
  ignore (expect state "in");
  let body = parse_expression state 0 in
  let binding =
    { name = identifier.text;
      value;
      recursive;
      bspan = span start value.span.finish }
  in
  expression (Let (binding, body)) start body.span.finish

and function_expression state start =
  let first = parse_pattern state in
  let rec parameters values =
    if starts_pattern (current state) then
      parameters (parse_pattern state :: values)
    else List.rev values
  in
  let parameters = parameters [first] in
  ignore (expect state "->");
  let body = parse_expression state 0 in
  List.fold_right
    (fun parameter body ->
      expression
        (Function (parameter, body))
        parameter.pspan.start
        body.span.finish)
    parameters body
  |> fun value -> { value with span = span start value.span.finish }

and cases state =
  ignore (take state "|");
  let rec collect values =
    let casepattern = parse_pattern state in
    ignore (expect state "->");
    let casebody = parse_expression state 6 in
    let branch =
      { casepattern;
        casebody;
        casespan = span casepattern.pspan.start casebody.span.finish }
    in
    match take state "|" with
    | Some _ -> collect (branch :: values)
    | None -> List.rev (branch :: values)
  in
  collect []

and function_cases state start =
  let parameter = pattern (Pvariable "$argument") start start in
  let target = expression (Variable "$argument") start start in
  let branches = cases state in
  let finish = (List.hd (List.rev branches)).casebody.span.finish in
  let body = expression (Match (target, branches)) start finish in
  expression (Function (parameter, body)) start finish

and conditional state start =
  let condition = parse_expression state 0 in
  ignore (expect state "then");
  let consequent = parse_expression state 0 in
  ignore (expect state "else");
  let alternate = parse_expression state 0 in
  expression
    (Conditional (condition, consequent, alternate))
    start
    alternate.span.finish

and match_expression state start =
  let target = parse_expression state 0 in
  ignore (expect state "with");
  let branches = cases state in
  let finish = (List.hd (List.rev branches)).casebody.span.finish in
  expression (Match (target, branches)) start finish

let parse source =
  let state =
    { source; tokens = Array.of_list (lex source); index = 0 }
  in
  let value = parse_expression state 0 in
  let token = current state in
  if token.kind <> Eof then
    raise
      (Error
         ( Printf.sprintf "unexpected token %S" token.text,
           token.start,
           token.finish ));
  value
