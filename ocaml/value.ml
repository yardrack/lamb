open Syntax

type environment = (string * value ref) list

and value =
  | Vint of int64
  | Vfloat of float
  | Vbool of bool
  | Vstring of string
  | Vchar of char
  | Vunit
  | Vtuple of value list
  | Vlist of value list
  | Vconstructor of string * value option
  | Vclosure of pattern * expression * environment
  | Vconstructorfunction of string

type block = {
  address : int64;
  tag : string;
  fields : int64 list;
}

type layout = {
  word : int64;
  immediate : bool;
  blocks : block list;
}

let rec format = function
  | Vint value -> Int64.to_string value
  | Vfloat value -> Printf.sprintf "%.12g" value
  | Vbool value -> string_of_bool value
  | Vstring value -> Printf.sprintf "%S" value
  | Vchar value -> Printf.sprintf "'%c'" value
  | Vunit -> "()"
  | Vtuple items ->
      "(" ^ String.concat ", " (List.map format items) ^ ")"
  | Vlist items ->
      "[" ^ String.concat "; " (List.map format items) ^ "]"
  | Vconstructor (name, None) -> name
  | Vconstructor (name, Some argument) -> name ^ " " ^ format argument
  | Vclosure _ | Vconstructorfunction _ -> "<fun>"

let rec equal left right =
  match left, right with
  | Vint left, Vint right -> left = right
  | Vfloat left, Vfloat right -> left = right
  | Vbool left, Vbool right -> left = right
  | Vstring left, Vstring right -> left = right
  | Vchar left, Vchar right -> left = right
  | Vunit, Vunit -> true
  | Vtuple left, Vtuple right | Vlist left, Vlist right ->
      List.length left = List.length right && List.for_all2 equal left right
  | Vconstructor (ln, la), Vconstructor (rn, ra) ->
      ln = rn
      && begin
           match la, ra with
           | None, None -> true
           | Some left, Some right -> equal left right
           | _ -> false
         end
  | _ -> false

let compare left right =
  match left, right with
  | Vint left, Vint right -> Int64.compare left right
  | Vfloat left, Vfloat right -> Stdlib.compare left right
  | Vstring left, Vstring right -> String.compare left right
  | Vchar left, Vchar right -> Char.compare left right
  | _ -> invalid_arg "unordered runtime values"

let encode_integer value =
  Int64.logor (Int64.shift_left value 1) 1L

let decode_integer value =
  if Int64.logand value 1L = 0L then invalid_arg "pointer-shaped word";
  Int64.shift_right value 1

let word_bits value =
  let buffer = Bytes.make 64 '0' in
  for index = 0 to 63 do
    let shift = 63 - index in
    if Int64.logand value (Int64.shift_left 1L shift) <> 0L then
      Bytes.set buffer index '1'
  done;
  Bytes.to_string buffer

let representation value =
  let next = ref 4096L in
  let blocks = ref [] in
  let allocate tag fields =
    let address = !next in
    next := Int64.add !next (Int64.of_int (8 * (List.length fields + 1)));
    blocks := { address; tag; fields } :: !blocks;
    address
  in
  let rec encode = function
    | Vint value -> encode_integer value
    | Vbool false | Vunit | Vconstructor ("None", None) -> 1L
    | Vbool true -> 3L
    | Vchar value -> encode_integer (Int64.of_int (Char.code value))
    | Vfloat value ->
        allocate "float" [Int64.bits_of_float value]
    | Vstring value ->
        let fields =
          value
          |> String.to_seq
          |> Seq.map (fun character -> Int64.of_int (Char.code character))
          |> List.of_seq
        in
        allocate "string" fields
    | Vtuple items ->
        allocate "tuple" (List.map encode items)
    | Vlist items -> encode_list items
    | Vconstructor (name, argument) ->
        let fields =
          match argument with None -> [] | Some value -> [encode value]
        in
        allocate name fields
    | Vclosure _ -> allocate "closure" []
    | Vconstructorfunction name -> allocate ("constructor:" ^ name) []
  and encode_list = function
    | [] -> 1L
    | head :: tail ->
        let headword = encode head in
        let tailword = encode_list tail in
        allocate "cons" [headword; tailword]
  in
  let word = encode value in
  { word;
    immediate = Int64.logand word 1L = 1L;
    blocks = List.rev !blocks }

let hexadecimal word =
  Printf.sprintf "0x%016Lx" word

let grouped_bits word =
  let bits = word_bits word in
  let pieces = ref [] in
  for index = 0 to 7 do
    pieces := String.sub bits (index * 8) 8 :: !pieces
  done;
  String.concat " " (List.rev !pieces)

let layout_lines layout =
  let header =
    [ "word " ^ Int64.to_string layout.word;
      "shape " ^ if layout.immediate then "immediate" else "pointer" ]
  in
  let blocks =
    List.map
      (fun block ->
        Printf.sprintf
          "0x%Lx %s [%s]"
          block.address
          block.tag
          (String.concat ", " (List.map Int64.to_string block.fields)))
      layout.blocks
  in
  header @ blocks
