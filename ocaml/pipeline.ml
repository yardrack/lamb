type stage = {
  key : string;
  name : string;
  phase : string;
  note : string;
  output : string;
  lines : int;
}

type result = {
  source : string;
  syntax : Syntax.expression;
  inference : Infer.result;
  analysis : Analysis.report;
  ir : Ir.expression;
  optimization : Optimize.result;
  bytecode : Machine.instruction list;
  value : Value.value;
  stages : stage list;
}

let descriptions =
  [ "source", "Source", "surface syntax",
    "The parser recognizes functional expressions with literals, collections, constructors and pattern matching.";
    "typedtree", "Typedtree", "type checking",
    "Hindley-Milner inference annotates expressions and generalizes let-bound values.";
    "lambda", "Lambda", "language lowering",
    "Typed syntax becomes a compact functional intermediate representation.";
    "clambda", "Clambda", "closure conversion",
    "Functions expose captured environments and generic closure calls.";
    "cmm", "Cmm", "machine lowering",
    "Tagged arithmetic and allocation become explicit machine operations.";
    "assembly", "Assembly", "instruction selection",
    "The final sketch shows x86-64 operations over tagged values." ]

let line_count text =
  if text = "" then 0
  else
    String.fold_left
      (fun count character -> if character = '\n' then count + 1 else count)
      1
      text

let stage outputs (key, name, phase, note) =
  let output = List.assoc key outputs in
  { key; name; phase; note; output; lines = line_count output }

let compile source =
  let syntax = Parser.parse source in
  let inference = Infer.infer syntax in
  let analysis = Analysis.analyze syntax in
  let ir = Ir.lower syntax in
  let optimization = Optimize.optimize ir in
  let bytecode = Machine.compile optimization.Optimize.expression in
  begin
    match Machine.verify bytecode with
    | Result.Ok () -> ()
    | Result.Error message -> raise (Machine.Error message)
  end;
  let value = Eval.run syntax in
  let outputs =
    [ "source", String.trim source;
      "typedtree", Lower.typedtree inference syntax;
      "lambda", Lower.lambda syntax;
      "clambda", Lower.clambda syntax;
      "cmm", Lower.cmm syntax;
      "assembly", Lower.assembly syntax ]
  in
  { source;
    syntax;
    inference;
    analysis;
    ir;
    optimization;
    bytecode;
    value;
    stages = List.map (stage outputs) descriptions }

let result_type result =
  Infer.format_result result.inference

let result_value result =
  Value.format result.value

let machine_value result =
  Machine.execute [] [] result.bytecode

let machine_result result =
  Machine.format_runtime (machine_value result)

let find_stage result name =
  List.find_opt (fun stage -> stage.key = String.lowercase_ascii name) result.stages

let sample =
  "let rec sum n =\n  if n = 0 then 0\n  else n + sum (n - 1)\nin\nsum 6"
