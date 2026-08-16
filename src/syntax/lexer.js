export class LambError extends Error {
  constructor(message, start = 0, end = start + 1, code = "E_SYNTAX") {
    super(message)
    this.name = "LambError"
    this.start = start
    this.end = end
    this.code = code
  }
}

const keywords = new Set(["let", "rec", "in", "fun", "function", "if", "then", "else", "true", "false", "match", "with", "begin", "end", "mod"])
const pairs = new Set(["->", "::", "<=", ">=", "<>", "&&", "||"])
const singles = new Set(["+", "-", "*", "/", "=", "<", ">", "^", "(", ")", "[", "]", ",", ";", "|"])

function escapeValue(source, index, quote) {
  const escaped = source[index + 1]
  if (escaped === undefined) throw new LambError(`unterminated ${quote === "\"" ? "string" : "character"} literal`, index)
  const simple = { n: "\n", r: "\r", t: "\t", b: "\b", "\\": "\\", "\"": "\"", "'": "'" }
  if (escaped in simple) return { value: simple[escaped], next: index + 2 }
  if (escaped === "x") {
    const digits = source.slice(index + 2, index + 4)
    if (!/^[0-9a-fA-F]{2}$/.test(digits)) throw new LambError("invalid hexadecimal escape", index, index + 4)
    return { value: String.fromCodePoint(Number.parseInt(digits, 16)), next: index + 4 }
  }
  if (/[0-9]/.test(escaped)) {
    const digits = source.slice(index + 1, index + 4)
    if (!/^[0-9]{3}$/.test(digits) || Number(digits) > 255) throw new LambError("invalid decimal escape", index, index + 4)
    return { value: String.fromCodePoint(Number(digits)), next: index + 4 }
  }
  return { value: escaped, next: index + 2 }
}

function quoted(source, start, quote) {
  let index = start + 1
  let value = ""
  while (index < source.length) {
    if (source[index] === quote) return { value, end: index + 1 }
    if (source[index] === "\n" && quote === "'") throw new LambError("unterminated character literal", start, index)
    if (source[index] === "\\") {
      const escaped = escapeValue(source, index, quote)
      value += escaped.value
      index = escaped.next
    } else {
      value += source[index]
      index += 1
    }
  }
  throw new LambError(`unterminated ${quote === "\"" ? "string" : "character"} literal`, start, source.length)
}

export function tokenize(source) {
  const tokens = []
  let index = 0
  const push = (kind, value, start, end = index) => tokens.push({ kind, value, start, end })
  while (index < source.length) {
    const character = source[index]
    if (/\s/.test(character)) {
      index += 1
      continue
    }
    if (source.startsWith("(*", index)) {
      const start = index
      let depth = 1
      index += 2
      while (index < source.length && depth) {
        if (source.startsWith("(*", index)) {
          depth += 1
          index += 2
        } else if (source.startsWith("*)", index)) {
          depth -= 1
          index += 2
        } else index += 1
      }
      if (depth) throw new LambError("unterminated comment", start, source.length)
      continue
    }
    const start = index
    if (character === "\"") {
      const result = quoted(source, start, "\"")
      index = result.end
      push("string", result.value, start)
      continue
    }
    if (character === "'") {
      const result = quoted(source, start, "'")
      if ([...result.value].length !== 1) throw new LambError("character literals contain exactly one character", start, result.end)
      index = result.end
      push("character", result.value, start)
      continue
    }
    const pair = source.slice(index, index + 2)
    if (pairs.has(pair)) {
      index += 2
      push("operator", pair, start)
      continue
    }
    if (/[0-9]/.test(character)) {
      index += 1
      while (/[0-9_]/.test(source[index] || "")) index += 1
      let kind = "integer"
      if (source[index] === "." && /[0-9]/.test(source[index + 1] || "")) {
        kind = "float"
        index += 1
        while (/[0-9_]/.test(source[index] || "")) index += 1
      }
      const raw = source.slice(start, index)
      if (raw.endsWith("_")) throw new LambError("invalid numeric literal", start, index)
      push(kind, raw.replaceAll("_", ""), start)
      continue
    }
    if (/[A-Za-z_]/.test(character)) {
      index += 1
      while (/[A-Za-z0-9_']/.test(source[index] || "")) index += 1
      const value = source.slice(start, index)
      const kind = keywords.has(value) ? "keyword" : /^[A-Z]/.test(value) ? "constructor" : "identifier"
      push(kind, value, start)
      continue
    }
    if (singles.has(character)) {
      index += 1
      push("()[],;|".includes(character) ? "punctuation" : "operator", character, start)
      continue
    }
    throw new LambError(`unexpected character ${JSON.stringify(character)}`, start, start + 1)
  }
  tokens.push({ kind: "eof", value: "", start: source.length, end: source.length })
  return tokens
}
