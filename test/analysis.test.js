import test from "node:test"
import assert from "node:assert/strict"
import { analyze, compile, parse } from "../src/compiler.js"

test("analysis counts recursion functions and applications", () => {
  const report = compile("let rec loop value = if value = 0 then 0 else loop (value - 1) in loop 2").analysis
  assert.equal(report.metrics.recursive, 1)
  assert.equal(report.metrics.functions, 1)
  assert.equal(report.metrics.applications, 2)
})

test("analysis detects closure free variables", () => {
  const ast = parse("fun value -> value + offset")
  assert.deepEqual(analyze(ast).free, ["offset"])
})

test("analysis accepts exhaustive boolean matches", () => {
  const report = compile("match true with | true -> 1 | false -> 0").analysis
  assert.equal(report.warnings.length, 0)
})

test("analysis warns about incomplete option matches", () => {
  const report = compile("match Some 1 with | Some value -> value").analysis
  assert.equal(report.warnings[0].code, "W_NONEXHAUSTIVE")
})

test("analysis warns after a catch-all pattern", () => {
  const report = compile("match true with | value -> 1 | false -> 0").analysis
  assert.equal(report.warnings[0].code, "W_REDUNDANT_CASE")
})
