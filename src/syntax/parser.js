import { LambError, tokenize } from "./lexer.js"

const binary = {
  ";": [5, "right"],
  "||": [10, "left"],
  "&&": [20, "left"],
  "=": [30, "left"],
  "<>": [30, "left"],
  "<": [30, "left"],
  ">": [30, "left"],
  "<=": [30, "left"],
  ">=": [30, "left"],
  "::": [35, "right"],
  "+": [40, "left"],
  "-": [40, "left"],
  "^": [40, "right"],
  "*": [50, "left"],
  "/": [50, "left"],
  mod: [50, "left"]
}

export class Parser {
  constructor(source) {
    this.source = source
    this.tokens = tokenize(source)
    this.index = 0
  }

  current() {
    return this.tokens[this.index]
  }

  take(value) {
    if (this.current().value !== value) return null
    return this.tokens[this.index++]
  }

  expect(value) {
    const token = this.take(value)
    if (token) return token
    const current = this.current()
    throw new LambError(`expected ${JSON.stringify(value)} but found ${JSON.stringify(current.value || "end of input")}`, current.start, current.end)
  }

  name() {
    const token = this.current()
    if (token.kind !== "identifier") throw new LambError("expected an identifier", token.start, token.end)
    this.index += 1
    return token
  }

  parse() {
    const expression = this.expression(0)
    const current = this.current()
    if (current.kind !== "eof") throw new LambError(`unexpected token ${JSON.stringify(current.value)}`, current.start, current.end)
    return expression
  }

  expression(minimum = 0) {
    let left = this.prefix()
    while (true) {
      const token = this.current()
      if (this.startsAtom(token) && 80 >= minimum) {
        const argument = this.expression(81)
        left = { kind: "apply", callee: left, argument, start: left.start, end: argument.end }
        continue
      }
      const entry = binary[token.value]
      if (!entry || entry[0] < minimum) break
      this.index += 1
      const [precedence, associativity] = entry
      const right = this.expression(associativity === "right" ? precedence : precedence + 1)
      if (token.value === ";") left = { kind: "sequence", first: left, second: right, start: left.start, end: right.end }
      else if (token.value === "::") left = { kind: "cons", head: left, tail: right, start: left.start, end: right.end }
      else left = { kind: "binary", operator: token.value, left, right, start: left.start, end: right.end }
    }
    return left
  }

  prefix() {
    const token = this.current()
    if (this.take("let")) return this.letExpression(token.start)
    if (this.take("fun")) return this.functionExpression(token.start)
    if (this.take("function")) return this.functionCases(token.start)
    if (this.take("if")) return this.ifExpression(token.start)
    if (this.take("match")) return this.matchExpression(token.start)
    if (this.take("begin")) {
      const value = this.expression(0)
      const close = this.expect("end")
      return { ...value, start: token.start, end: close.end }
    }
    if (this.take("-")) {
      const value = this.expression(70)
      return { kind: "unary", operator: "-", value, start: token.start, end: value.end }
    }
    if (token.kind === "integer") {
      this.index += 1
      return { kind: "integer", value: BigInt(token.value), start: token.start, end: token.end }
    }
    if (token.kind === "float") {
      this.index += 1
      return { kind: "float", value: Number(token.value), start: token.start, end: token.end }
    }
    if (token.kind === "string" || token.kind === "character") {
      this.index += 1
      return { kind: token.kind, value: token.value, start: token.start, end: token.end }
    }
    if (token.value === "true" || token.value === "false") {
      this.index += 1
      return { kind: "boolean", value: token.value === "true", start: token.start, end: token.end }
    }
    if (token.kind === "identifier" || token.kind === "constructor") {
      this.index += 1
      return { kind: token.kind === "constructor" ? "constructor" : "variable", name: token.value, start: token.start, end: token.end }
    }
    if (this.take("[")) return this.listExpression(token.start)
    if (this.take("(")) return this.parenthesized(token.start)
    throw new LambError(`expected an expression but found ${JSON.stringify(token.value || "end of input")}`, token.start, token.end)
  }

  parenthesized(start) {
    if (this.take(")")) return { kind: "unit", start, end: this.tokens[this.index - 1].end }
    const first = this.expression(0)
    if (!this.take(",")) {
      const close = this.expect(")")
      return { ...first, start, end: close.end }
    }
    const items = [first]
    do items.push(this.expression(6)); while (this.take(","))
    const close = this.expect(")")
    return { kind: "tuple", items, start, end: close.end }
  }

  listExpression(start) {
    if (this.take("]")) return { kind: "list", items: [], start, end: this.tokens[this.index - 1].end }
    const items = [this.expression(6)]
    while (this.take(";")) items.push(this.expression(6))
    const close = this.expect("]")
    return { kind: "list", items, start, end: close.end }
  }

  letExpression(start) {
    const recursive = Boolean(this.take("rec"))
    const name = this.name()
    const parameters = []
    while (this.startsPattern(this.current())) parameters.push(this.pattern())
    this.expect("=")
    let value = this.expression(0)
    for (const parameter of parameters.reverse()) value = { kind: "function", parameter, body: value, start: parameter.start, end: value.end }
    this.expect("in")
    const body = this.expression(0)
    return { kind: "let", name: name.value, value, body, recursive, start, end: body.end }
  }

  functionExpression(start) {
    const parameters = [this.pattern()]
    while (this.startsPattern(this.current())) parameters.push(this.pattern())
    this.expect("->")
    let body = this.expression(0)
    for (const parameter of parameters.reverse()) body = { kind: "function", parameter, body, start: parameter.start, end: body.end }
    return { ...body, start }
  }

  functionCases(start) {
    const parameter = { kind: "pvariable", name: "$argument", start, end: start }
    const target = { kind: "variable", name: "$argument", start, end: start }
    const cases = this.cases()
    return { kind: "function", parameter, body: { kind: "match", target, cases, start, end: cases.at(-1).body.end }, start, end: cases.at(-1).body.end }
  }

  ifExpression(start) {
    const condition = this.expression(0)
    this.expect("then")
    const consequent = this.expression(0)
    this.expect("else")
    const alternate = this.expression(0)
    return { kind: "if", condition, consequent, alternate, start, end: alternate.end }
  }

  matchExpression(start) {
    const target = this.expression(0)
    this.expect("with")
    const cases = this.cases()
    return { kind: "match", target, cases, start, end: cases.at(-1).body.end }
  }

  cases() {
    this.take("|")
    const cases = []
    do {
      const pattern = this.pattern()
      this.expect("->")
      const body = this.expression(6)
      cases.push({ pattern, body, start: pattern.start, end: body.end })
    } while (this.take("|"))
    if (!cases.length) {
      const current = this.current()
      throw new LambError("expected at least one pattern case", current.start, current.end)
    }
    return cases
  }

  pattern() {
    let left = this.patternAtom()
    if (this.take("::")) {
      const tail = this.pattern()
      left = { kind: "pcons", head: left, tail, start: left.start, end: tail.end }
    }
    return left
  }

  patternAtom() {
    const token = this.current()
    if (token.kind === "identifier") {
      this.index += 1
      return token.value === "_" ? { kind: "pwildcard", start: token.start, end: token.end } : { kind: "pvariable", name: token.value, start: token.start, end: token.end }
    }
    if (token.kind === "integer") {
      this.index += 1
      return { kind: "pinteger", value: BigInt(token.value), start: token.start, end: token.end }
    }
    if (token.kind === "string" || token.kind === "character") {
      this.index += 1
      return { kind: `p${token.kind}`, value: token.value, start: token.start, end: token.end }
    }
    if (token.value === "true" || token.value === "false") {
      this.index += 1
      return { kind: "pboolean", value: token.value === "true", start: token.start, end: token.end }
    }
    if (token.kind === "constructor") {
      this.index += 1
      const argument = this.startsPattern(this.current()) ? this.patternAtom() : null
      return { kind: "pconstructor", name: token.value, argument, start: token.start, end: argument?.end ?? token.end }
    }
    if (this.take("[")) {
      const close = this.expect("]")
      return { kind: "pnil", start: token.start, end: close.end }
    }
    if (this.take("(")) {
      if (this.take(")")) return { kind: "punit", start: token.start, end: this.tokens[this.index - 1].end }
      const first = this.pattern()
      if (!this.take(",")) {
        const close = this.expect(")")
        return { ...first, start: token.start, end: close.end }
      }
      const items = [first]
      do items.push(this.pattern()); while (this.take(","))
      const close = this.expect(")")
      return { kind: "ptuple", items, start: token.start, end: close.end }
    }
    throw new LambError(`expected a pattern but found ${JSON.stringify(token.value || "end of input")}`, token.start, token.end)
  }

  startsPattern(token) {
    return token.kind === "identifier" || token.kind === "integer" || token.kind === "string" || token.kind === "character" || token.kind === "constructor" || ["true", "false", "(", "["].includes(token.value)
  }

  startsAtom(token) {
    return token.kind === "integer" || token.kind === "float" || token.kind === "string" || token.kind === "character" || token.kind === "identifier" || token.kind === "constructor" || ["true", "false", "(", "[", "begin"].includes(token.value)
  }
}

export function parse(source) {
  return new Parser(source).parse()
}
