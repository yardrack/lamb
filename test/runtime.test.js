import test from "node:test"
import assert from "node:assert/strict"
import { compile, representation } from "../src/compiler.js"

test("evaluates tail destructuring over lists", () => {
  const result = compile("match [1; 2; 3] with | [] -> 0 | head :: tail -> head")
  assert.equal(result.result, "1")
})

test("evaluates recursive factorial", () => {
  const result = compile("let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 6")
  assert.equal(result.result, "720")
})

test("evaluates lexical closure capture", () => {
  const result = compile("let offset = 40 in let add = fun value -> offset + value in add 2")
  assert.equal(result.result, "42")
})

test("evaluates option construction and matching", () => {
  const result = compile("match Some 42 with | None -> 0 | Some value -> value")
  assert.equal(result.result, "42")
})

test("formats compound values using OCaml syntax", () => {
  assert.equal(compile("([1; 2], Some true)").result, "([1; 2], Some true)")
})

test("represents integers as tagged immediate words", () => {
  const layout = representation(compile("42").value)
  assert.equal(layout.word, 85n)
  assert.equal(layout.immediate, true)
})

test("represents tuples as aligned heap blocks", () => {
  const layout = representation(compile("(1, true)").value)
  assert.equal(layout.word & 7n, 0n)
  assert.equal(layout.blocks[0].fields.length, 2)
})
