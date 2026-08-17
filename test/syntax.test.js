import test from "node:test"
import assert from "node:assert/strict"
import { LambError, parse, tokenize } from "../src/compiler.js"

test("lexer decodes string and character escapes", () => {
  const tokens = tokenize("\"a\\n\" 'x' '\\120'")
  assert.deepEqual(tokens.slice(0, -1).map(token => token.value), ["a\n", "x", "x"])
})

test("lexer recognizes floats constructors and cons", () => {
  const tokens = tokenize("Some 1.25 :: values")
  assert.deepEqual(tokens.slice(0, -1).map(token => token.kind), ["constructor", "float", "operator", "identifier"])
})

test("lexer rejects malformed character literals", () => {
  assert.throws(() => tokenize("'ab'"), LambError)
})

test("parser makes cons right associative", () => {
  const ast = parse("1 :: 2 :: []")
  assert.equal(ast.kind, "cons")
  assert.equal(ast.tail.kind, "cons")
})

test("parser distinguishes list separators from sequences", () => {
  const ast = parse("[1; 2; 3]")
  assert.equal(ast.kind, "list")
  assert.equal(ast.items.length, 3)
})

test("parser retains tuple patterns", () => {
  const ast = parse("fun (left, right) -> left")
  assert.equal(ast.parameter.kind, "ptuple")
  assert.equal(ast.parameter.items[1].name, "right")
})

test("parser lowers function cases to a match", () => {
  const ast = parse("function | None -> 0 | Some value -> value")
  assert.equal(ast.kind, "function")
  assert.equal(ast.body.kind, "match")
  assert.equal(ast.body.cases.length, 2)
})
