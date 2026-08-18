import { readdirSync } from "node:fs"
import { join } from "node:path"
import { spawnSync } from "node:child_process"

function files(directory) {
  return readdirSync(directory, { withFileTypes: true }).flatMap(entry => entry.isDirectory() ? files(join(directory, entry.name)) : [join(directory, entry.name)])
}

const targets = ["src", "test", "tool"].flatMap(files).filter(file => file.endsWith(".js"))
for (const target of ["cli.js", "core.js", "lamb", "bin/lamb", ...targets]) {
  const result = spawnSync(process.execPath, ["--check", target], { encoding: "utf8" })
  if (result.status !== 0) {
    process.stderr.write(result.stderr)
    process.exit(result.status || 1)
  }
}
process.stdout.write(`checked ${targets.length + 4} javascript files\n`)
