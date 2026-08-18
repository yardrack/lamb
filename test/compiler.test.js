import test from "node:test"
import assert from "node:assert/strict"
import { spawnSync } from "node:child_process"
import { fileURLToPath } from "node:url"
import { LambError, compile, decodeInteger, encodeInteger, parse, samples, tokenize, wordBits } from "../src/compiler.js"

const directory = fileURLToPath(new URL(".", import.meta.url))

function invoke(arguments_, input = "") {
  return spawnSync(process.execPath, ["../bin/lamb", ...arguments_], { cwd: directory, input, encoding: "utf8" })
}

test("tokenizer accepts nested comments and separators", () => {
  const tokens = tokenize("let x = 1 (* outer (* inner *) *) in x + 2")
  assert.deepEqual(tokens.filter(token => token.kind !== "eof").map(token => token.value), ["let", "x", "=", "1", "in", "x", "+", "2"])
})

test("parser honors arithmetic precedence", () => {
  const ast = parse("1 + 2 * 3")
  assert.equal(ast.operator, "+")
  assert.equal(ast.right.operator, "*")
})

test("compiler infers polymorphic identity usage", () => {
  const result = compile("let identity = fun value -> value in let number = identity 42 in identity true")
  assert.equal(result.type, "bool")
  assert.equal(result.stages.length, 6)
})

test("compiler reports incompatible branch types", () => {
  assert.throws(() => compile("if true then 1 else false"), LambError)
})

test("compiler reports unbound identifiers", () => {
  assert.throws(() => compile("missing + 1"), /unbound value missing/)
})

test("recursive functions infer without explicit annotations", () => {
  const result = compile("let rec sum n = if n = 0 then 0 else n + sum (n - 1) in sum 6")
  assert.equal(result.type, "int")
  assert.match(result.stages[3].output, /Uletrec/)
})

test("closure conversion records captured values", () => {
  const result = compile("let offset = 7 in let add = fun value -> value + offset in add 35")
  assert.match(result.stages[3].output, /environment \[offset\]/)
})

test("tagged integers round trip across signed values", () => {
  for (const value of [-1048576n, -1n, 0n, 1n, 42n, 1048576n]) assert.equal(decodeInteger(encodeInteger(value)), value)
})

test("tagged integer words always expose an immediate marker", () => {
  for (const value of [-99n, 0n, 99n]) assert.equal(wordBits(encodeInteger(value)).at(-1), "1")
})

test("every bundled program completes all stages", () => {
  for (const sample of samples) {
    const result = compile(sample.source)
    assert.equal(result.stages.length, 6)
    assert.ok(result.stages.every(stage => stage.output.length > 0))
  }
})

test("cli prints a selected stage from a file", () => {
  const result = invoke(["../sample.ml", "--stage", "lambda", "--no-color"])
  assert.equal(result.status, 0)
  assert.doesNotMatch(result.stdout, /^Lamb$/m)
  assert.doesNotMatch(result.stdout, /v1\.0\.0/)
  assert.match(result.stdout, /Lfunction value/)
  assert.doesNotMatch(result.stdout, /Texp_let/)
})

test("cli accepts source from stdin", () => {
  const result = invoke(["--stage", "cmm", "--no-color"], "let x = 20 in x + 22\n")
  assert.equal(result.status, 0)
  assert.match(result.stdout, /Input\s+stdin/)
  assert.match(result.stdout, /x \+ v2 - 1L/)
})

test("cli emits tagged words as json", () => {
  const result = invoke(["--word", "-42", "--json"])
  assert.equal(result.status, 0)
  const value = JSON.parse(result.stdout)
  assert.equal(value.encoded, "-83")
  assert.equal(value.binary.at(-1), "1")
})

test("cli reports source locations and compilation exit status", () => {
  const result = invoke(["--no-color"], "if true then 1 else false\n")
  assert.equal(result.status, 1)
  assert.match(result.stderr, /stdin:1:1/)
  assert.match(result.stderr, /\^/)
})

test("cli rejects invalid options with usage exit status", () => {
  const result = invoke(["--unknown"])
  assert.equal(result.status, 2)
  assert.match(result.stderr, /unknown option --unknown/)
})
