import { readdir, readFile } from "node:fs/promises"
import { compile } from "../src/compiler.js"

const names = (await readdir("examples")).filter(name => name.endsWith(".ml")).sort()
for (const name of names) {
  const source = await readFile(`examples/${name}`, "utf8")
  const result = compile(source)
  if (result.stages.length !== 6 || result.stages.some(stage => !stage.output)) throw new Error(`${name} did not complete the lowering path`)
  process.stdout.write(`${name.padEnd(16)} ${result.type.padEnd(18)} ${result.result}\n`)
}
