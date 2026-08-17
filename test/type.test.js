import test from "node:test"
import assert from "node:assert/strict"
import { LambError, compile } from "../src/compiler.js"

test("infers homogeneous lists", () => {
  assert.equal(compile("[1; 2; 3]").type, "int list")
})

test("infers tuples without collapsing item types", () => {
  assert.equal(compile("(1, true, \"value\")").type, "int * bool * string")
})

test("infers polymorphic option constructors", () => {
  assert.equal(compile("Some (fun value -> value)").type, "('a -> 'a) option")
})

test("infers pattern-bound values", () => {
  assert.equal(compile("let first = fun (left, right) -> left in first (1, true)").type, "int")
})

test("infers recursive list traversal", () => {
  const source = "let rec length values = match values with | [] -> 0 | head :: tail -> 1 + length tail in length [1; 2]"
  assert.equal(compile(source).type, "int")
})

test("rejects heterogeneous list items", () => {
  assert.throws(() => compile("[1; true]"), LambError)
})

test("rejects duplicate names within one pattern", () => {
  assert.throws(() => compile("fun (value, value) -> value"), /bound several times/)
})

test("rejects constructor arity mistakes", () => {
  assert.throws(() => compile("match Some 1 with | None value -> value | Some value -> value"), /expects no argument/)
})

test("requires unit on the left side of sequencing", () => {
  assert.throws(() => compile("1; 2"), LambError)
})
