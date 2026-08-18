type mode =
  | Compile
  | Run
  | Analyze
  | Represent
  | Tokens
  | Word

type options = {
  mutable mode : mode;
  mutable stage : string;
  mutable input : string option;
  mutable integer : string option;
  mutable json : bool;
  mutable liststages : bool;
}

let options =
  { mode = Compile;
    stage = "all";
    input = None;
    integer = None;
    json = false;
    liststages = false }

let escape text =
  let buffer = Buffer.create (String.length text + 8) in
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | character -> Buffer.add_char buffer character)
    text;
  Buffer.contents buffer

let jsonstring text =
  "\"" ^ escape text ^ "\""

let read_channel channel =
  let buffer = Buffer.create 256 in
  begin
    try
      while true do
        Buffer.add_channel buffer channel 4096
      done
    with End_of_file -> ()
  end;
  Buffer.contents buffer

let read_input () =
  match options.input with
  | Some path ->
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in channel)
        (fun () -> path, read_channel channel)
  | None -> "stdin", read_channel stdin

let set_input path =
  match options.input with
  | None -> options.input <- Some path
  | Some _ -> raise (Arg.Bad ("unexpected second input " ^ path))

let specification =
  [ "-s", Arg.String (fun value -> options.stage <- String.lowercase_ascii value),
    "Print one compiler stage";
    "--stage", Arg.String (fun value -> options.stage <- String.lowercase_ascii value),
    "Print one compiler stage";
    "-r", Arg.Unit (fun () -> options.mode <- Run), "Evaluate the program";
    "--run", Arg.Unit (fun () -> options.mode <- Run), "Evaluate the program";
    "-a", Arg.Unit (fun () -> options.mode <- Analyze), "Analyze the program";
    "--analyze", Arg.Unit (fun () -> options.mode <- Analyze), "Analyze the program";
    "--repr", Arg.Unit (fun () -> options.mode <- Represent),
    "Inspect the runtime representation";
    "--tokens", Arg.Unit (fun () -> options.mode <- Tokens),
    "Print the token stream";
    "-w", Arg.String (fun value -> options.mode <- Word; options.integer <- Some value),
    "Inspect a tagged integer";
    "--word", Arg.String (fun value -> options.mode <- Word; options.integer <- Some value),
    "Inspect a tagged integer";
    "-j", Arg.Unit (fun () -> options.json <- true), "Emit JSON";
    "--json", Arg.Unit (fun () -> options.json <- true), "Emit JSON";
    "--no-color", Arg.Unit (fun () -> ()), "Disable ANSI color";
    "--list", Arg.Unit (fun () -> options.liststages <- true), "List compiler stages" ]

let usage =
  "Lamb native OCaml compiler observatory\n\nUsage: lambnative [file] [options]"

let print_stage result stage =
  Printf.printf
    "%-8s %s\n%-8s %s\n%-8s %d\n\n"
    "Input"
    (match options.input with Some path -> Filename.basename path | None -> "stdin")
    "Type"
    (Pipeline.result_type result)
    "Nodes"
    result.Pipeline.analysis.Analysis.metrics.nodes;
  Printf.printf
    "%s\n\n%s\n"
    stage.Pipeline.name
    stage.Pipeline.output

let print_compile result =
  let stages =
    if options.stage = "all" then result.Pipeline.stages
    else
      match Pipeline.find_stage result options.stage with
      | Some stage -> [stage]
      | None -> raise (Arg.Bad ("unknown stage " ^ options.stage))
  in
  if options.json then
    let stages =
      stages
      |> List.map (fun stage ->
             Printf.sprintf
               "{\"key\":%s,\"name\":%s,\"phase\":%s,\"lines\":%d,\"output\":%s}"
               (jsonstring stage.Pipeline.key)
               (jsonstring stage.Pipeline.name)
               (jsonstring stage.Pipeline.phase)
               stage.Pipeline.lines
               (jsonstring stage.Pipeline.output))
      |> String.concat ","
    in
    Printf.printf
      "{\"type\":%s,\"nodes\":%d,\"stages\":[%s]}\n"
      (jsonstring (Pipeline.result_type result))
      result.Pipeline.analysis.Analysis.metrics.nodes
      stages
  else
    List.iter (print_stage result) stages

let print_run result =
  let datatype = Pipeline.result_type result in
  let value = Pipeline.result_value result in
  if options.json then
    Printf.printf
      "{\"type\":%s,\"value\":%s}\n"
      (jsonstring datatype)
      (jsonstring value)
  else Printf.printf "Type   %s\nValue  %s\n" datatype value

let print_analysis result =
  let report = result.Pipeline.analysis in
  if options.json then
    let metrics =
      Analysis.metric_lines report.Analysis.metrics
      |> List.map (fun (name, value) ->
             Printf.sprintf "%s:%d" (jsonstring name) value)
      |> String.concat ","
    in
    let free =
      report.Analysis.free |> List.map jsonstring |> String.concat ","
    in
    let warnings =
      report.Analysis.warnings
      |> List.map (fun warning ->
             Printf.sprintf
               "{\"code\":%s,\"message\":%s}"
               (jsonstring warning.Analysis.code)
               (jsonstring warning.Analysis.message))
      |> String.concat ","
    in
    Printf.printf
      "{\"type\":%s,\"metrics\":{%s},\"free\":[%s],\"warnings\":[%s]}\n"
      (jsonstring (Pipeline.result_type result))
      metrics
      free
      warnings
  else Printf.printf "%s\n" (Analysis.format report)

let print_representation result =
  let layout = Value.representation result.Pipeline.value in
  if options.json then
    let blocks =
      layout.Value.blocks
      |> List.map (fun block ->
             Printf.sprintf
               "{\"address\":%s,\"tag\":%s,\"fields\":[%s]}"
               (jsonstring (Printf.sprintf "0x%Lx" block.Value.address))
               (jsonstring block.Value.tag)
               (String.concat
                  ","
                  (List.map
                     (fun field -> jsonstring (Int64.to_string field))
                     block.Value.fields)))
      |> String.concat ","
    in
    Printf.printf
      "{\"value\":%s,\"word\":%s,\"immediate\":%b,\"blocks\":[%s]}\n"
      (jsonstring (Pipeline.result_value result))
      (jsonstring (Int64.to_string layout.Value.word))
      layout.Value.immediate
      blocks
  else
    Printf.printf
      "Value  %s\n%s\n"
      (Pipeline.result_value result)
      (String.concat "\n" (Value.layout_lines layout))

let print_tokens source =
  let tokens =
    Token.lex source
    |> List.filter (fun token -> token.Token.kind <> Token.Eof)
  in
  if options.json then
    let tokens =
      tokens
      |> List.map (fun token ->
             Printf.sprintf
               "{\"kind\":%s,\"value\":%s,\"start\":%d,\"finish\":%d}"
               (jsonstring (Token.kind_name token.Token.kind))
               (jsonstring token.Token.text)
               token.Token.start
               token.Token.finish)
      |> String.concat ","
    in
    Printf.printf "{\"tokens\":[%s]}\n" tokens
  else
    List.iter
      (fun token ->
        Printf.printf
          "%5d  %-12s %S\n"
          token.Token.start
          (Token.kind_name token.Token.kind)
          token.Token.text)
      tokens

let print_word text =
  let integer = Int64.of_string text in
  let encoded = Value.encode_integer integer in
  if options.json then
    Printf.printf
      "{\"integer\":%s,\"encoded\":%s,\"hexadecimal\":%s,\"binary\":%s,\"tag\":1}\n"
      (jsonstring (Int64.to_string integer))
      (jsonstring (Int64.to_string encoded))
      (jsonstring (Value.hexadecimal encoded))
      (jsonstring (Value.word_bits encoded))
  else
    Printf.printf
      "OCaml int  %Ld\nEncoded    %Ld\nHex        %s\nBits       %s\nLow bit    1\n"
      integer
      encoded
      (Value.hexadecimal encoded)
      (Value.grouped_bits encoded)

let line_column source offset =
  let line = ref 1 in
  let column = ref 1 in
  for index = 0 to min offset (String.length source) - 1 do
    if source.[index] = '\n' then begin incr line; column := 1 end
    else incr column
  done;
  !line, !column

let report_error input source message start finish =
  let line, column = line_column source start in
  let lines = String.split_on_char '\n' source in
  let text =
    match List.nth_opt lines (line - 1) with Some text -> text | None -> ""
  in
  Printf.eprintf "error: %s\n  %s:%d:%d\n\n" message input line column;
  Printf.eprintf "%d | %s\n  | %s%s\n" line text (String.make (column - 1) ' ') (String.make (max 1 (finish - start)) '^')

let execute () =
  Arg.parse specification set_input usage;
  if options.liststages then begin
    List.iter
      (fun (key, _, phase, _) -> Printf.printf "%-11s %s\n" key phase)
      Pipeline.descriptions;
    0
  end else if options.mode = Word then begin
    match options.integer with
    | Some text -> print_word text; 0
    | None -> raise (Arg.Bad "--word requires an integer")
  end else
    let input, source = read_input () in
    if String.trim source = "" then raise (Arg.Bad "input is empty");
    try
      if options.mode = Tokens then begin
        print_tokens source;
        0
      end else begin
        let result = Pipeline.compile source in
        begin
          match options.mode with
          | Compile -> print_compile result
          | Run -> print_run result
          | Analyze -> print_analysis result
          | Represent -> print_representation result
          | Tokens | Word -> assert false
        end;
        0
      end
    with
    | Token.Error (message, start, finish) ->
        report_error input source message start finish;
        1
    | Infer.Error (message, location) | Eval.Error (message, location) ->
        report_error input source message location.Syntax.start location.Syntax.finish;
        1

let () =
  try exit (execute ()) with
  | Arg.Bad message -> Printf.eprintf "error: %s\n" message; exit 2
  | Failure message -> Printf.eprintf "error: %s\n" message; exit 2
