type kind =
  | Keyword
  | Identifier
  | Constructor
  | Integer
  | Float
  | String
  | Character
  | Operator
  | Punctuation
  | Eof

type token = {
  kind : kind;
  text : string;
  start : int;
  finish : int;
}

exception Error of string * int * int

let keywords =
  [ "let"; "rec"; "in"; "fun"; "function"; "if"; "then"; "else";
    "true"; "false"; "match"; "with"; "begin"; "end"; "mod" ]

let pairs =
  [ "->"; "::"; "<="; ">="; "<>"; "&&"; "||" ]

let singles = "+-*/=<>^()[],;|"

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_digit character =
  character >= '0' && character <= '9'

let is_lower character =
  character >= 'a' && character <= 'z'

let is_upper character =
  character >= 'A' && character <= 'Z'

let is_letter character =
  is_lower character || is_upper character || character = '_'

let is_name character =
  is_letter character || is_digit character || character = '\''

let member character text =
  let rec search index =
    index < String.length text
    && (text.[index] = character || search (index + 1))
  in
  search 0

let starts source index text =
  let length = String.length text in
  index + length <= String.length source
  && String.sub source index length = text

let decode_escape source index =
  if index + 1 >= String.length source then
    raise (Error ("unterminated escape", index, index + 1));
  match source.[index + 1] with
  | 'n' -> ('\n', index + 2)
  | 'r' -> ('\r', index + 2)
  | 't' -> ('\t', index + 2)
  | 'b' -> ('\b', index + 2)
  | '\\' -> ('\\', index + 2)
  | '\'' -> ('\'', index + 2)
  | '"' -> ('"', index + 2)
  | 'x' ->
      if index + 3 >= String.length source then
        raise (Error ("invalid hexadecimal escape", index, index + 2));
      let digits = String.sub source (index + 2) 2 in
      begin
        try (Char.chr (int_of_string ("0x" ^ digits)), index + 4)
        with Failure _ ->
          raise (Error ("invalid hexadecimal escape", index, index + 4))
      end
  | first when is_digit first ->
      if index + 3 >= String.length source then
        raise (Error ("invalid decimal escape", index, index + 2));
      let digits = String.sub source (index + 1) 3 in
      begin
        try
          let code = int_of_string digits in
          if code > 255 then
            raise (Error ("invalid decimal escape", index, index + 4));
          (Char.chr code, index + 4)
        with Failure _ ->
          raise (Error ("invalid decimal escape", index, index + 4))
      end
  | character -> (character, index + 2)

let quoted source start quote =
  let buffer = Buffer.create 16 in
  let rec consume index =
    if index >= String.length source then
      raise (Error ("unterminated literal", start, String.length source));
    let character = source.[index] in
    if character = quote then (Buffer.contents buffer, index + 1)
    else if character = '\n' && quote = '\'' then
      raise (Error ("unterminated character literal", start, index))
    else if character = '\\' then
      let decoded, next = decode_escape source index in
      Buffer.add_char buffer decoded;
      consume next
    else begin
      Buffer.add_char buffer character;
      consume (index + 1)
    end
  in
  consume (start + 1)

let strip_separators value =
  String.to_seq value
  |> Seq.filter (fun character -> character <> '_')
  |> String.of_seq

let lex source =
  let length = String.length source in
  let make kind text start finish = { kind; text; start; finish } in
  let rec comment start index depth =
    if index >= length then
      raise (Error ("unterminated comment", start, length));
    if starts source index "(*" then comment start (index + 2) (depth + 1)
    else if starts source index "*)" then
      if depth = 1 then index + 2 else comment start (index + 2) (depth - 1)
    else comment start (index + 1) depth
  in
  let rec number start index =
    if index < length && (is_digit source.[index] || source.[index] = '_') then
      number start (index + 1)
    else
      let decimal =
        index + 1 < length && source.[index] = '.' && is_digit source.[index + 1]
      in
      let rec fraction cursor =
        if cursor < length
           && (is_digit source.[cursor] || source.[cursor] = '_')
        then fraction (cursor + 1)
        else cursor
      in
      let finish = if decimal then fraction (index + 1) else index in
      let raw = String.sub source start (finish - start) in
      if raw.[String.length raw - 1] = '_' then
        raise (Error ("invalid numeric literal", start, finish));
      let kind = if decimal then Float else Integer in
      (make kind (strip_separators raw) start finish, finish)
  in
  let rec name start index =
    if index < length && is_name source.[index] then name start (index + 1)
    else
      let text = String.sub source start (index - start) in
      let kind =
        if List.mem text keywords then Keyword
        else if is_upper text.[0] then Constructor
        else Identifier
      in
      (make kind text start index, index)
  in
  let rec scan index tokens =
    if index >= length then
      List.rev (make Eof "" length length :: tokens)
    else if is_space source.[index] then scan (index + 1) tokens
    else if starts source index "(*" then
      scan (comment index (index + 2) 1) tokens
    else
      let start = index in
      let character = source.[index] in
      if character = '"' || character = '\'' then
        let text, finish = quoted source start character in
        if character = '\'' && String.length text <> 1 then
          raise (Error ("character literals contain exactly one character", start, finish));
        let kind = if character = '"' then String else Character in
        scan finish (make kind text start finish :: tokens)
      else if is_digit character then
        let token, finish = number start (index + 1) in
        scan finish (token :: tokens)
      else if is_letter character then
        let token, finish = name start (index + 1) in
        scan finish (token :: tokens)
      else
        let pair =
          if index + 1 < length then String.sub source index 2 else ""
        in
        if List.mem pair pairs then
          scan (index + 2) (make Operator pair start (index + 2) :: tokens)
        else if member character singles then
          let text = String.make 1 character in
          let kind =
            if member character "()[],;|" then Punctuation else Operator
          in
          scan (index + 1) (make kind text start (index + 1) :: tokens)
        else
          raise
            (Error
               (Printf.sprintf "unexpected character %C" character, start, start + 1))
  in
  scan 0 []

let kind_name = function
  | Keyword -> "keyword"
  | Identifier -> "identifier"
  | Constructor -> "constructor"
  | Integer -> "integer"
  | Float -> "float"
  | String -> "string"
  | Character -> "character"
  | Operator -> "operator"
  | Punctuation -> "punctuation"
  | Eof -> "eof"
