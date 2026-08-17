import test from "node:test"
import assert from "node:assert/strict"
import { spawnSync } from "node:child_process"
import { fileURLToPath } from "node:url"

const directory = fileURLToPath(new URL(".", import.meta.url))
const invoke = (arguments_, input) => spawnSync(process.execPath, ["../bin/lamb", ...arguments_], { cwd: directory, input, encoding: "utf8" })

test("run mode evaluates source", () => {
  const result = invoke(["--run", "--no-color"], "21 * 2")
  assert.equal(result.status, 0)
  assert.match(result.stdout, /Value\s+42/)
})

test("analysis mode emits metrics as json", () => {
  const result = invoke(["--analyze", "--json"], "fun value -> value")
  const output = JSON.parse(result.stdout)
  assert.equal(output.metrics.functions, 1)
})

test("representation mode emits blocks as json", () => {
  const result = invoke(["--repr", "--json"], "[1; 2]")
  const output = JSON.parse(result.stdout)
  assert.equal(output.layout.immediate, false)
  assert.equal(output.layout.blocks[0].tag, "list")
})

test("token mode exposes source offsets", () => {
  const result = invoke(["--tokens", "--json"], "let x = 1 in x")
  const output = JSON.parse(result.stdout)
  assert.equal(output.tokens[0].start, 0)
  assert.equal(output.tokens.at(-1).value, "x")
})
