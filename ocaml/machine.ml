type wordkind =
  | Integer
  | Boolean
  | Character
  | Unit

type runtime =
  | Word of wordkind * int64
  | Float of float
  | Text of string
  | Tuple of runtime list
  | List of runtime list
  | Constructor of string * runtime option
  | Closure of Ir.pattern * instruction list * environment

and environment = (string * runtime ref) list

and instruction =
  | Push of Ir.constant
  | Load of string
  | Bind of string
  | Unbind of string
  | Bindrecursive of string * Ir.pattern * instruction list
  | Unary of string
  | Binary of string
  | Buildtuple of int
  | Buildlist of int
  | Buildcons
  | Buildconstructor of string * bool
  | Makeclosure of Ir.pattern * instruction list
  | Call
  | Branch of instruction list * instruction list
  | Cases of (Ir.pattern * instruction list) list
  | Pop

exception Error of string

let word = function
  | Ir.Int value -> Word (Integer, Value.encode_integer value)
  | Ir.Bool value -> Word (Boolean, if value then 3L else 1L)
  | Ir.Char value ->
      Word (Character, Value.encode_integer (Int64.of_int (Char.code value)))
  | Ir.Unit -> Word (Unit, 1L)
  | Ir.Float value -> Float value
  | Ir.Text value -> Text value

let payload = function
  | Word (Integer, value) -> Value.decode_integer value
  | _ -> raise (Error "expected a tagged integer")

let truth = function
  | Word (Boolean, value) -> value <> 1L
  | _ -> raise (Error "expected a tagged boolean")

let text = function
  | Text value -> value
  | _ -> raise (Error "expected a string block")

let rec equal left right =
  match left, right with
  | Word (leftkind, left), Word (rightkind, right) ->
      leftkind = rightkind && left = right
  | Float left, Float right -> left = right
  | Text left, Text right -> left = right
  | Tuple left, Tuple right | List left, List right ->
      List.length left = List.length right && List.for_all2 equal left right
  | Constructor (leftname, leftarg), Constructor (rightname, rightarg) ->
      leftname = rightname
      && begin
           match leftarg, rightarg with
           | None, None -> true
           | Some left, Some right -> equal left right
           | _ -> false
         end
  | _ -> false

let compare left right =
  match left, right with
  | Word (Integer, left), Word (Integer, right) ->
      Int64.compare (Value.decode_integer left) (Value.decode_integer right)
  | Word (Character, left), Word (Character, right) -> Int64.compare left right
  | Float left, Float right -> Stdlib.compare left right
  | Text left, Text right -> String.compare left right
  | _ -> raise (Error "runtime values are not ordered")

let unary operator = function
  | value when operator = "-" ->
      Word (Integer, Value.encode_integer (Int64.neg (payload value)))
  | _ -> raise (Error ("unknown unary instruction " ^ operator))

let binary operator left right =
  match operator with
  | "+" -> Word (Integer, Value.encode_integer (Int64.add (payload left) (payload right)))
  | "-" -> Word (Integer, Value.encode_integer (Int64.sub (payload left) (payload right)))
  | "*" -> Word (Integer, Value.encode_integer (Int64.mul (payload left) (payload right)))
  | "/" ->
      let divisor = payload right in
      if divisor = 0L then raise (Error "division by zero");
      Word (Integer, Value.encode_integer (Int64.div (payload left) divisor))
  | "mod" ->
      let divisor = payload right in
      if divisor = 0L then raise (Error "division by zero");
      Word (Integer, Value.encode_integer (Int64.rem (payload left) divisor))
  | "^" -> Text (text left ^ text right)
  | "=" -> Word (Boolean, if equal left right then 3L else 1L)
  | "<>" -> Word (Boolean, if equal left right then 1L else 3L)
  | "<" -> Word (Boolean, if compare left right < 0 then 3L else 1L)
  | ">" -> Word (Boolean, if compare left right > 0 then 3L else 1L)
  | "<=" -> Word (Boolean, if compare left right <= 0 then 3L else 1L)
  | ">=" -> Word (Boolean, if compare left right >= 0 then 3L else 1L)
  | "&&" -> Word (Boolean, if truth left && truth right then 3L else 1L)
  | "||" -> Word (Boolean, if truth left || truth right then 3L else 1L)
  | _ -> raise (Error ("unknown binary instruction " ^ operator))

let rec compile (expression : Ir.expression) =
  match expression with
  | Ir.Const value -> [Push value]
  | Ir.Var name -> [Load name]
  | Ir.Unary (operator, argument) -> compile argument @ [Unary operator]
  | Ir.Primitive (operator, [left; right]) ->
      compile left @ compile right @ [Binary operator]
  | Ir.Primitive (operator, _) ->
      raise (Error ("primitive " ^ operator ^ " has unsupported arity"))
  | Ir.Let (name, value, body) ->
      compile value @ [Bind name] @ compile body @ [Unbind name]
  | Ir.Letrec (name, Ir.Function (_, parameter, functionbody), body) ->
      [Bindrecursive (name, parameter, compile functionbody)]
      @ compile body
      @ [Unbind name]
  | Ir.Letrec (name, value, body) ->
      compile value @ [Bind name] @ compile body @ [Unbind name]
  | Ir.Function (_, parameter, body) -> [Makeclosure (parameter, compile body)]
  | Ir.Apply (callee, argument) -> compile callee @ compile argument @ [Call]
  | Ir.If (condition, consequent, alternate) ->
      compile condition @ [Branch (compile consequent, compile alternate)]
  | Ir.Switch (target, cases) ->
      compile target
      @ [Cases (List.map (fun (pattern, body) -> pattern, compile body) cases)]
  | Ir.Tuplevalue items ->
      List.concat_map compile items @ [Buildtuple (List.length items)]
  | Ir.Listvalue items ->
      List.concat_map compile items @ [Buildlist (List.length items)]
  | Ir.Consvalue (head, tail) -> compile head @ compile tail @ [Buildcons]
  | Ir.Constructor (name, None) -> [Buildconstructor (name, false)]
  | Ir.Constructor (name, Some argument) ->
      compile argument @ [Buildconstructor (name, true)]
  | Ir.Sequence (first, second) -> compile first @ [Pop] @ compile second

let take count stack =
  let rec loop remaining values stack =
    if remaining = 0 then values, stack
    else
      match stack with
      | value :: stack -> loop (remaining - 1) (value :: values) stack
      | [] -> raise (Error "operand stack underflow")
  in
  loop count [] stack

let lookup environment name =
  match List.assoc_opt name environment with
  | Some value -> !value
  | None -> raise (Error ("unbound bytecode value " ^ name))

let constant_runtime = function
  | Ir.Constant constant -> word constant
  | _ -> raise (Error "pattern is not constant")

let rec match_pattern (pattern : Ir.pattern) value bindings =
  match pattern, value with
  | Ir.Wildcard, _ -> Some bindings
  | Ir.Binder name, value -> Some ((name, ref value) :: bindings)
  | Ir.Constant expected, value when equal (word expected) value -> Some bindings
  | Ir.Nil, List [] -> Some bindings
  | Ir.Cons (head, tail), List (first :: rest) ->
      begin
        match match_pattern head first bindings with
        | None -> None
        | Some bindings -> match_pattern tail (List rest) bindings
      end
  | Ir.Tuple patterns, Tuple values when List.length patterns = List.length values ->
      List.fold_left2
        (fun result pattern value ->
          match result with
          | None -> None
          | Some bindings -> match_pattern pattern value bindings)
        (Some bindings)
        patterns
        values
  | Ir.Construct (name, None), Constructor (actual, None) when name = actual ->
      Some bindings
  | Ir.Construct (name, Some pattern), Constructor (actual, Some value)
    when name = actual -> match_pattern pattern value bindings
  | _ -> None

let rec execute environment stack instructions =
  match instructions with
  | [] ->
      begin
        match stack with
        | value :: _ -> value
        | [] -> raise (Error "empty result stack")
      end
  | instruction :: remaining ->
      begin
        match instruction with
        | Push constant -> execute environment (word constant :: stack) remaining
        | Load name -> execute environment (lookup environment name :: stack) remaining
        | Bind name ->
            begin
              match stack with
              | value :: stack ->
                  execute ((name, ref value) :: environment) stack remaining
              | [] -> raise (Error "bind requires a value")
            end
        | Unbind name ->
            execute (List.remove_assoc name environment) stack remaining
        | Bindrecursive (name, parameter, body) ->
            let placeholder = ref (Word (Unit, 1L)) in
            let nested = (name, placeholder) :: environment in
            placeholder := Closure (parameter, body, nested);
            execute nested stack remaining
        | Unary operator ->
            begin
              match stack with
              | value :: stack ->
                  execute environment (unary operator value :: stack) remaining
              | [] -> raise (Error "unary instruction requires an operand")
            end
        | Binary operator ->
            begin
              match stack with
              | right :: left :: stack ->
                  execute environment (binary operator left right :: stack) remaining
              | _ -> raise (Error "binary instruction requires two operands")
            end
        | Buildtuple count ->
            let values, stack = take count stack in
            execute environment (Tuple values :: stack) remaining
        | Buildlist count ->
            let values, stack = take count stack in
            execute environment (List values :: stack) remaining
        | Buildcons ->
            begin
              match stack with
              | List tail :: head :: stack ->
                  execute environment (List (head :: tail) :: stack) remaining
              | _ -> raise (Error "cons requires a value and list")
            end
        | Buildconstructor (name, argument) ->
            if argument then
              begin
                match stack with
                | value :: stack ->
                    execute environment (Constructor (name, Some value) :: stack) remaining
                | [] -> raise (Error "constructor requires an argument")
              end
            else execute environment (Constructor (name, None) :: stack) remaining
        | Makeclosure (parameter, body) ->
            execute environment (Closure (parameter, body, environment) :: stack) remaining
        | Call ->
            begin
              match stack with
              | argument :: Closure (parameter, body, closureenvironment) :: stack ->
                  begin
                    match match_pattern parameter argument [] with
                    | Some bindings ->
                        let result = execute (bindings @ closureenvironment) [] body in
                        execute environment (result :: stack) remaining
                    | None -> raise (Error "closure parameter match failed")
                  end
              | _ -> raise (Error "call requires a closure and argument")
            end
        | Branch (consequent, alternate) ->
            begin
              match stack with
              | condition :: stack ->
                  let branch = if truth condition then consequent else alternate in
                  let result = execute environment [] branch in
                  execute environment (result :: stack) remaining
              | [] -> raise (Error "branch requires a condition")
            end
        | Cases cases ->
            begin
              match stack with
              | target :: stack ->
                  let rec choose = function
                    | [] -> raise (Error "bytecode pattern matching failed")
                    | (pattern, body) :: cases ->
                        begin
                          match match_pattern pattern target [] with
                          | Some bindings -> execute (bindings @ environment) [] body
                          | None -> choose cases
                        end
                  in
                  execute environment (choose cases :: stack) remaining
              | [] -> raise (Error "switch requires a target")
            end
        | Pop ->
            begin
              match stack with
              | _ :: stack -> execute environment stack remaining
              | [] -> raise (Error "pop on an empty stack")
            end
      end

let run expression =
  execute [] [] (compile expression)

let rec format_runtime = function
  | Word (Integer, value) -> Int64.to_string (Value.decode_integer value)
  | Word (Boolean, value) -> string_of_bool (value <> 1L)
  | Word (Character, value) ->
      Printf.sprintf "'%c'" (Char.chr (Int64.to_int (Value.decode_integer value)))
  | Word (Unit, _) -> "()"
  | Float value -> Printf.sprintf "%.12g" value
  | Text value -> Printf.sprintf "%S" value
  | Tuple items -> "(" ^ String.concat ", " (List.map format_runtime items) ^ ")"
  | List items -> "[" ^ String.concat "; " (List.map format_runtime items) ^ "]"
  | Constructor (name, None) -> name
  | Constructor (name, Some argument) -> name ^ " " ^ format_runtime argument
  | Closure _ -> "<closure>"

let name = function
  | Push constant -> "push " ^ Ir.constant_text constant
  | Load name -> "load " ^ name
  | Bind name -> "bind " ^ name
  | Unbind name -> "unbind " ^ name
  | Bindrecursive (name, _, _) -> "bindrec " ^ name
  | Unary operator -> "unary " ^ operator
  | Binary operator -> "binary " ^ operator
  | Buildtuple count -> Printf.sprintf "tuple %d" count
  | Buildlist count -> Printf.sprintf "list %d" count
  | Buildcons -> "cons"
  | Buildconstructor (constructor, argument) ->
      Printf.sprintf "constructor %s %d" constructor (if argument then 1 else 0)
  | Makeclosure _ -> "closure"
  | Call -> "call"
  | Branch _ -> "branch"
  | Cases cases -> Printf.sprintf "switch %d" (List.length cases)
  | Pop -> "pop"

let disassemble instructions =
  instructions
  |> List.mapi (fun index instruction -> Printf.sprintf "%04d  %s" index (name instruction))
  |> String.concat "\n"

let stack_effect = function
  | Push _ | Load _ | Makeclosure _ -> 1
  | Bind _ | Pop -> -1
  | Unbind _ | Bindrecursive _ -> 0
  | Unary _ -> 0
  | Binary _ | Buildcons | Call -> -1
  | Buildtuple count | Buildlist count -> 1 - count
  | Buildconstructor (_, argument) -> if argument then 0 else 1
  | Branch _ | Cases _ -> 0

let verify instructions =
  let rec check depth minimum = function
    | [] -> depth, minimum
    | instruction :: remaining ->
        let depth = depth + stack_effect instruction in
        check depth (min minimum depth) remaining
  in
  let final, minimum = check 0 0 instructions in
  if minimum < 0 then Result.Error "bytecode stack underflow"
  else if final <> 1 then Result.Error (Printf.sprintf "bytecode leaves %d stack values" final)
  else Result.Ok ()
