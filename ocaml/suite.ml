let failures = ref 0

let check name condition =
  if condition then Printf.printf "ok %s\n" name
  else begin
    incr failures;
    Printf.eprintf "not ok %s\n" name
  end

let equal name expected actual =
  check name (expected = actual)

let raises name functionvalue =
  try
    ignore (functionvalue ());
    check name false
  with _ -> check name true

let compile source =
  Pipeline.compile source

let result source =
  compile source |> Pipeline.result_value

let datatype source =
  compile source |> Pipeline.result_type

let () =
  let tokens = Token.lex "let x = 1 (* outer (* inner *) *) in x + 2" in
  equal "nested comments" 9 (List.length tokens);
  let escapes = Token.lex "\"a\\n\" '\\120'" in
  equal "string escape" "a\n" (List.nth escapes 0).Token.text;
  equal "decimal escape" "x" (List.nth escapes 1).Token.text;
  raises "unterminated comment" (fun () -> Token.lex "(* open");
  raises "invalid character" (fun () -> Token.lex "'ab'");
  let precedence = Parser.parse "1 + 2 * 3" in
  begin
    match precedence.Syntax.expression with
    | Syntax.Binary ("+", _, right) ->
        check
          "operator precedence"
          (match right.Syntax.expression with Syntax.Binary ("*", _, _) -> true | _ -> false)
    | _ -> check "operator precedence" false
  end;
  let cons = Parser.parse "1 :: 2 :: []" in
  begin
    match cons.Syntax.expression with
    | Syntax.Cons (_, tail) ->
        check
          "cons associativity"
          (match tail.Syntax.expression with Syntax.Cons _ -> true | _ -> false)
    | _ -> check "cons associativity" false
  end;
  let functioncases = Parser.parse "function | None -> 0 | Some value -> value" in
  begin
    match functioncases.Syntax.expression with
    | Syntax.Function (_, body) ->
        check
          "function cases"
          (match body.Syntax.expression with Syntax.Match (_, [_; _]) -> true | _ -> false)
    | _ -> check "function cases" false
  end;
  equal "integer type" "int" (datatype "42");
  equal "boolean type" "bool" (datatype "true");
  equal "string type" "string" (datatype "\"value\"");
  equal "character type" "char" (datatype "'x'");
  equal "float type" "float" (datatype "1.25");
  equal "list type" "int list" (datatype "[1; 2; 3]");
  equal "tuple type" "int * bool * string" (datatype "(1, true, \"value\")");
  equal "option type" "int option" (datatype "Some 42");
  equal
    "polymorphic identity"
    "bool"
    (datatype "let identity = fun value -> value in let number = identity 42 in identity true");
  equal
    "recursive inference"
    "int"
    (datatype "let rec sum n = if n = 0 then 0 else n + sum (n - 1) in sum 6");
  equal
    "recursive list inference"
    "int"
    (datatype "let rec length values = match values with | [] -> 0 | value :: rest -> 1 + length rest in length [3; 5; 8]");
  equal
    "tuple parameter"
    "int"
    (datatype "let first = fun (left, right) -> left in first (1, true)");
  raises "heterogeneous list" (fun () -> compile "[1; true]");
  raises "incompatible branches" (fun () -> compile "if true then 1 else false");
  raises "unbound value" (fun () -> compile "missing + 1");
  raises "infinite type" (fun () -> compile "fun value -> value value");
  raises "duplicate pattern binding" (fun () -> compile "fun (value, value) -> value");
  equal "arithmetic evaluation" "42" (result "20 + 22");
  equal "precedence evaluation" "7" (result "1 + 2 * 3");
  equal "string concatenation" "\"typed lowering\"" (result "\"typed\" ^ \" lowering\"");
  equal "tuple formatting" "(42, true)" (result "(42, true)");
  equal "list formatting" "[1; 2; 3]" (result "[1; 2; 3]");
  equal "option formatting" "Some 42" (result "Some 42");
  equal
    "closure capture"
    "42"
    (result "let offset = 40 in let add = fun value -> offset + value in add 2");
  equal
    "recursive evaluation"
    "720"
    (result "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 6");
  equal
    "list matching"
    "1"
    (result "match [1; 2] with | [] -> 0 | head :: tail -> head");
  equal
    "option matching"
    "42"
    (result "match Some 42 with | None -> 0 | Some value -> value");
  let recursive = compile "let rec loop n = if n = 0 then 0 else loop (n - 1) in loop 2" in
  equal "recursive metric" 1 recursive.Pipeline.analysis.Analysis.metrics.recursivebindings;
  equal "function metric" 1 recursive.Pipeline.analysis.Analysis.metrics.functions;
  equal "application metric" 2 recursive.Pipeline.analysis.Analysis.metrics.applications;
  let exhaustive = compile "match true with | true -> 1 | false -> 0" in
  equal "exhaustive match" 0 (List.length exhaustive.Pipeline.analysis.Analysis.warnings);
  let incomplete = compile "match Some 1 with | Some value -> value" in
  equal "incomplete match" "W_NONEXHAUSTIVE" (List.hd incomplete.Pipeline.analysis.Analysis.warnings).Analysis.code;
  let closure = compile "let offset = 7 in fun value -> value + offset" in
  let clambda = (Pipeline.find_stage closure "clambda" |> Option.get).Pipeline.output in
  check "closure environment" (String.contains clambda '[' && String.contains clambda ']');
  let cmm = (Pipeline.find_stage (compile "let x = 20 in x + 22") "cmm" |> Option.get).Pipeline.output in
  check "tagged cmm addition" (String.contains cmm '-');
  let tagged = Value.encode_integer 42L in
  equal "tagged integer" 85L tagged;
  equal "tagged round trip" 42L (Value.decode_integer tagged);
  equal "immediate marker" '1' (Value.word_bits tagged).[63];
  let layout = Value.representation (compile "(1, true)").Pipeline.value in
  check "tuple pointer" (not layout.Value.immediate);
  equal "tuple block" "tuple" (List.hd layout.Value.blocks).Value.tag;
  equal "six stages" 6 (List.length (compile "42").Pipeline.stages);
  let arithmeticir = Parser.parse "let value = 20 in value + 22" |> Ir.lower in
  check "alpha renaming" (String.contains (Ir.format arithmeticir) '/');
  let folded = Optimize.optimize (Ir.lower (Parser.parse "20 + 22")) in
  equal "constant folding" 1 folded.Optimize.stats.Optimize.folds;
  equal "folded machine value" "42" (Machine.run folded.Optimize.expression |> Machine.format_runtime);
  let beta = Optimize.optimize (Ir.lower (Parser.parse "(fun value -> value + 1) 41")) in
  equal "beta reduction" 1 beta.Optimize.stats.Optimize.betareductions;
  equal "beta machine value" "42" (Machine.run beta.Optimize.expression |> Machine.format_runtime);
  let dead = Optimize.optimize (Ir.lower (Parser.parse "let unused = 21 in 42")) in
  equal "dead binding elimination" 1 dead.Optimize.stats.Optimize.deadbindings;
  let machinefactorial = compile "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 6" in
  equal "recursive bytecode" "720" (Pipeline.machine_result machinefactorial);
  let machinelength = compile "let rec length values = match values with | [] -> 0 | value :: rest -> 1 + length rest in length [3; 5; 8]" in
  equal "recursive list bytecode" "3" (Pipeline.machine_result machinelength);
  let machinelist = compile "match [1; 2] with | [] -> 0 | head :: tail -> head" in
  equal "pattern bytecode" "1" (Pipeline.machine_result machinelist);
  let machineoption = compile "match Some 42 with | None -> 0 | Some value -> value" in
  equal "constructor bytecode" "42" (Pipeline.machine_result machineoption);
  begin
    match Machine.verify (Machine.compile folded.Optimize.expression) with
    | Result.Ok () -> check "bytecode verification" true
    | Result.Error _ -> check "bytecode verification" false
  end;
  if !failures > 0 then exit 1
