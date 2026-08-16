import { analyze } from "./analysis/index.js"
import { assembly, clambda, cmm, lambda, typedtree } from "./backend/render.js"
import { evaluate, formatValue } from "./runtime/evaluate.js"
import { parse } from "./syntax/parser.js"
import { formatType, infer } from "./type/infer.js"

export const stages = [
  { key: "source", name: "Source", phase: "surface syntax", note: "The parser recognizes functional expressions with literals, collections, constructors and pattern matching." },
  { key: "typedtree", name: "Typedtree", phase: "type checking", note: "Hindley–Milner inference annotates expressions and generalizes let-bound values." },
  { key: "lambda", name: "Lambda", phase: "language lowering", note: "Typed syntax becomes a compact functional intermediate representation." },
  { key: "clambda", name: "Clambda", phase: "closure conversion", note: "Functions expose captured environments and generic closure calls." },
  { key: "cmm", name: "Cmm", phase: "machine lowering", note: "Tagged arithmetic and allocation become explicit machine operations." },
  { key: "assembly", name: "Assembly", phase: "instruction selection", note: "The final sketch shows x86-64 operations over tagged values." }
]

export function compile(source) {
  const ast = infer(parse(source))
  const report = analyze(ast)
  const output = { source: source.trim(), typedtree: typedtree(ast), lambda: lambda(ast), clambda: clambda(ast), cmm: cmm(ast), assembly: assembly(ast) }
  const value = evaluate(ast)
  return { ast, type: formatType(ast.type), nodes: report.metrics.nodes, analysis: report, value, result: formatValue(value), stages: stages.map(stage => ({ ...stage, output: output[stage.key], lines: output[stage.key].split("\n").length })) }
}

export const samples = [
  { name: "tagged arithmetic", source: "let x = 21 in let y = x * 2 in y + 1" },
  { name: "closure capture", source: "let offset = 7 in let add = fun value -> value + offset in add 35" },
  { name: "polymorphic identity", source: "let identity = fun value -> value in let number = identity 42 in identity true" },
  { name: "recursive function", source: "let rec sum n = if n = 0 then 0 else n + sum (n - 1) in sum 6" },
  { name: "list pattern", source: "let head = function | [] -> 0 | value :: rest -> value in head [42; 7]" },
  { name: "option pattern", source: "let unwrap = function | None -> 0 | Some value -> value in unwrap (Some 42)" }
]
