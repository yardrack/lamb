import { readFile } from "node:fs/promises"
import { basename } from "node:path"
import { compile, encodeInteger, LambError, stages, wordBits } from "../compiler.js"

const version = "1.0.0"
const names = new Map(stages.map(stage => [stage.key, stage.name]))

function colors(enabled) {
  const wrap = code => value => enabled ? `\u001b[${code}m${value}\u001b[0m` : String(value)
  return {
    bold: wrap("1"),
    dim: wrap("2"),
    blue: wrap("34"),
    cyan: wrap("36"),
    red: wrap("31"),
    yellow: wrap("33")
  }
}

function usage(style) {
  return `${style.bold("Lamb")}

Model the OCaml native compiler lowering path in a terminal.

${style.bold("Usage")}
  lamb [file] [options]
  command | lamb [options]
  lamb --word <integer>

${style.bold("Options")}
  -s, --stage <name>  Print one stage or all stages
  -w, --word <int>    Inspect a tagged machine word
  -j, --json          Emit a machine-readable trace
      --no-color      Disable ANSI styling
      --list          List available stages
  -h, --help          Print this help
  -v, --version       Print the version

${style.bold("Stages")}
  source, typedtree, lambda, clambda, cmm, assembly, all`
}

function parseArguments(arguments_) {
  const options = { stage: "all", file: null, word: null, json: false, color: true, help: false, version: false, list: false }
  for (let index = 0; index < arguments_.length; index += 1) {
    const argument = arguments_[index]
    if (argument === "-h" || argument === "--help") options.help = true
    else if (argument === "-v" || argument === "--version") options.version = true
    else if (argument === "-j" || argument === "--json") options.json = true
    else if (argument === "--no-color") options.color = false
    else if (argument === "--list") options.list = true
    else if (argument === "-s" || argument === "--stage") {
      index += 1
      if (!arguments_[index]) throw new Error(`${argument} requires a stage name`)
      options.stage = arguments_[index].toLowerCase()
    } else if (argument.startsWith("--stage=")) options.stage = argument.slice(8).toLowerCase()
    else if (argument === "-w" || argument === "--word") {
      index += 1
      if (!arguments_[index]) throw new Error(`${argument} requires an integer`)
      options.word = arguments_[index]
    } else if (argument.startsWith("--word=")) options.word = argument.slice(7)
    else if (argument.startsWith("-")) throw new Error(`unknown option ${argument}`)
    else if (options.file) throw new Error(`unexpected second input ${argument}`)
    else options.file = argument
  }
  if (!names.has(options.stage) && options.stage !== "all") throw new Error(`unknown stage ${options.stage}`)
  if (options.word !== null && options.file) throw new Error("--word cannot be combined with a source file")
  return options
}

async function readStdin() {
  const chunks = []
  for await (const chunk of process.stdin) chunks.push(chunk)
  return Buffer.concat(chunks).toString("utf8")
}

function groupedBits(bits) {
  return bits.match(/.{1,8}/g).join(" ")
}

function printWord(raw, style, json) {
  let value
  try {
    value = BigInt(raw)
  } catch {
    throw new Error(`${JSON.stringify(raw)} is not a signed decimal integer`)
  }
  const encoded = encodeInteger(value)
  const masked = encoded & ((1n << 64n) - 1n)
  const record = {
    integer: value.toString(),
    encoded: encoded.toString(),
    hexadecimal: `0x${masked.toString(16).padStart(16, "0")}`,
    binary: wordBits(encoded),
    tag: 1,
    representation: "immediate"
  }
  if (json) return process.stdout.write(`${JSON.stringify(record, null, 2)}\n`)
  process.stdout.write(`${style.bold("Lamb")} ${style.dim("/ tagged word")}\n\n`)
  process.stdout.write(`${style.dim("OCaml int ")} ${style.bold(record.integer)}\n`)
  process.stdout.write(`${style.dim("Encoded   ")} ${style.cyan(record.encoded)}\n`)
  process.stdout.write(`${style.dim("Hex       ")} ${record.hexadecimal}\n`)
  process.stdout.write(`${style.dim("Bits      ")} ${groupedBits(record.binary.slice(0, -1))}${style.blue(record.binary.at(-1))}\n`)
  process.stdout.write(`${style.dim("Low bit   ")} ${style.blue("1")} ${style.dim("· immediate value")}\n`)
}

function printStages(result, input, selected, style) {
  const shown = selected === "all" ? result.stages : result.stages.filter(stage => stage.key === selected)
  process.stdout.write(`${style.dim("Input   ")} ${input}\n`)
  process.stdout.write(`${style.dim("Type    ")} ${style.cyan(result.type)}\n`)
  process.stdout.write(`${style.dim("Nodes   ")} ${result.nodes}\n\n`)
  process.stdout.write(result.stages.map(stage => stage.key === selected || selected === "all" ? style.blue(stage.name) : style.dim(stage.name)).join(style.dim(" → ")) + "\n")
  for (const stage of shown) {
    const rule = "─".repeat(Math.max(2, 56 - stage.name.length - stage.phase.length))
    process.stdout.write(`\n${style.blue("┌─")} ${style.bold(stage.name)} ${style.dim(`· ${stage.phase} · ${stage.lines} lines`)} ${style.blue(rule)}\n`)
    const lines = stage.output.split("\n")
    for (const line of lines) process.stdout.write(`${style.blue("│")} ${line}\n`)
    process.stdout.write(`${style.blue("└─")} ${style.dim(stage.note)}\n`)
  }
}

function printJson(result, input, selected) {
  const shown = selected === "all" ? result.stages : result.stages.filter(stage => stage.key === selected)
  process.stdout.write(`${JSON.stringify({ input, type: result.type, nodes: result.nodes, stages: shown }, null, 2)}\n`)
}

function printError(problem, source, input, style) {
  process.stderr.write(`${style.red(style.bold("error"))}: ${problem.message}\n`)
  if (!(problem instanceof LambError) || !source) return
  const before = source.slice(0, problem.start).split("\n")
  const line = before.length
  const column = before.at(-1).length + 1
  const text = source.split("\n")[line - 1] || ""
  const gutter = String(line).length
  process.stderr.write(`${style.dim(`  ${input}:${line}:${column}`)}\n\n`)
  process.stderr.write(`${style.dim(`${String(line).padStart(gutter)} │`)} ${text}\n`)
  process.stderr.write(`${" ".repeat(gutter)} ${style.dim("│")} ${" ".repeat(column - 1)}${style.red("^".repeat(Math.max(1, problem.end - problem.start)))}\n`)
}

export async function run(arguments_ = process.argv.slice(2)) {
  let options
  try {
    options = parseArguments(arguments_)
  } catch (problem) {
    const style = colors(false)
    process.stderr.write(`${style.bold("error")}: ${problem.message}\nTry 'lamb --help' for usage.\n`)
    return 2
  }
  const enabled = options.color && !process.env.NO_COLOR && (process.stdout.isTTY || process.env.FORCE_COLOR === "1")
  const style = colors(enabled)
  if (options.help) {
    process.stdout.write(`${usage(style)}\n`)
    return 0
  }
  if (options.version) {
    process.stdout.write(`${version}\n`)
    return 0
  }
  if (options.list) {
    for (const stage of stages) process.stdout.write(`${stage.key.padEnd(11)} ${stage.phase}\n`)
    return 0
  }
  if (options.word !== null) {
    try {
      printWord(options.word, style, options.json)
      return 0
    } catch (problem) {
      printError(problem, "", "", style)
      return 2
    }
  }
  let source = ""
  let input = "stdin"
  try {
    if (options.file) {
      source = await readFile(options.file, "utf8")
      input = basename(options.file)
    } else if (!process.stdin.isTTY) source = await readStdin()
    else {
      process.stderr.write(`${usage(style)}\n`)
      return 2
    }
    if (!source.trim()) throw new Error("input is empty")
    const result = compile(source)
    if (options.json) printJson(result, input, options.stage)
    else printStages(result, input, options.stage, style)
    return 0
  } catch (problem) {
    printError(problem, source, input, style)
    return problem instanceof LambError ? 1 : 2
  }
}
