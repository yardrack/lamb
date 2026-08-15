const keywords = new Set(["let", "rec", "in", "fun", "if", "then", "else", "true", "false"])

export class LambError extends Error {
  constructor(message, start = 0, end = start + 1) {
    super(message)
    this.name = "LambError"
    this.start = start
    this.end = end
  }
}

export function tokenize(source) {
  const tokens = []
  let index = 0
  const push = (kind, value, start, end = index) => tokens.push({ kind, value, start, end })
  while (index < source.length) {
    const char = source[index]
    if (/\s/.test(char)) {
      index += 1
      continue
    }
    if (source.startsWith("(*", index)) {
      const start = index
      index += 2
      let depth = 1
      while (index < source.length && depth > 0) {
        if (source.startsWith("(*", index)) {
          depth += 1
          index += 2
        } else if (source.startsWith("*)", index)) {
          depth -= 1
          index += 2
        } else {
          index += 1
        }
      }
      if (depth > 0) throw new LambError("unterminated comment", start, source.length)
      continue
    }
    const start = index
    const pair = source.slice(index, index + 2)
    if (["->", "<=", ">=", "<>", "&&", "||"].includes(pair)) {
      index += 2
      push("operator", pair, start)
      continue
    }
    if (/[0-9]/.test(char)) {
      index += 1
      while (index < source.length && /[0-9_]/.test(source[index])) index += 1
      const value = source.slice(start, index)
      if (value.endsWith("_")) throw new LambError("invalid integer literal", start, index)
      push("integer", value.replaceAll("_", ""), start)
      continue
    }
    if (/[A-Za-z_]/.test(char)) {
      index += 1
      while (index < source.length && /[A-Za-z0-9_']/.test(source[index])) index += 1
      const value = source.slice(start, index)
      push(keywords.has(value) ? "keyword" : "identifier", value, start)
      continue
    }
    if ("+-*/=<>()".includes(char)) {
      index += 1
      const kind = "()".includes(char) ? "punctuation" : "operator"
      push(kind, char, start)
      continue
    }
    throw new LambError(`unexpected character ${JSON.stringify(char)}`, start, start + 1)
  }
  tokens.push({ kind: "eof", value: "", start: source.length, end: source.length })
  return tokens
}

class Parser {
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
    if (!token) {
      const current = this.current()
      throw new LambError(`expected ${JSON.stringify(value)} but found ${JSON.stringify(current.value || "end of input")}`, current.start, current.end)
    }
    return token
  }

  identifier() {
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

  expression(minimum) {
    let left = this.prefix()
    while (true) {
      const token = this.current()
      if (this.startsAtom(token) && 80 >= minimum) {
        const right = this.expression(81)
        left = { kind: "apply", callee: left, argument: right, start: left.start, end: right.end }
        continue
      }
      const precedence = this.precedence(token.value)
      if (precedence < minimum) break
      this.index += 1
      const right = this.expression(precedence + 1)
      left = { kind: "binary", operator: token.value, left, right, start: left.start, end: right.end }
    }
    return left
  }

  prefix() {
    const token = this.current()
    if (this.take("let")) return this.letExpression(token.start)
    if (this.take("fun")) return this.functionExpression(token.start)
    if (this.take("if")) return this.ifExpression(token.start)
    if (this.take("-")) {
      const value = this.expression(70)
      return { kind: "unary", operator: "-", value, start: token.start, end: value.end }
    }
    if (token.kind === "integer") {
      this.index += 1
      return { kind: "integer", value: BigInt(token.value), start: token.start, end: token.end }
    }
    if (token.value === "true" || token.value === "false") {
      this.index += 1
      return { kind: "boolean", value: token.value === "true", start: token.start, end: token.end }
    }
    if (token.kind === "identifier") {
      this.index += 1
      return { kind: "variable", name: token.value, start: token.start, end: token.end }
    }
    if (this.take("(")) {
      if (this.take(")")) return { kind: "unit", start: token.start, end: this.tokens[this.index - 1].end }
      const value = this.expression(0)
      const close = this.expect(")")
      return { ...value, start: token.start, end: close.end }
    }
    throw new LambError(`expected an expression but found ${JSON.stringify(token.value || "end of input")}`, token.start, token.end)
  }

  letExpression(start) {
    const recursive = Boolean(this.take("rec"))
    const name = this.identifier()
    const parameters = []
    while (this.current().kind === "identifier") parameters.push(this.identifier())
    this.expect("=")
    let value = this.expression(0)
    for (const parameter of parameters.reverse()) {
      value = { kind: "function", parameter: parameter.value, body: value, start: parameter.start, end: value.end }
    }
    this.expect("in")
    const body = this.expression(0)
    return { kind: "let", name: name.value, value, body, recursive, start, end: body.end }
  }

  functionExpression(start) {
    const parameters = [this.identifier()]
    while (this.current().kind === "identifier") parameters.push(this.identifier())
    this.expect("->")
    let body = this.expression(0)
    for (const parameter of parameters.reverse()) {
      body = { kind: "function", parameter: parameter.value, body, start: parameter.start, end: body.end }
    }
    return { ...body, start }
  }

  ifExpression(start) {
    const condition = this.expression(0)
    this.expect("then")
    const consequent = this.expression(0)
    this.expect("else")
    const alternate = this.expression(0)
    return { kind: "if", condition, consequent, alternate, start, end: alternate.end }
  }

  startsAtom(token) {
    return token.kind === "integer" || token.kind === "identifier" || ["true", "false", "("].includes(token.value)
  }

  precedence(operator) {
    return { "||": 10, "&&": 20, "=": 30, "<>": 30, "<": 30, ">": 30, "<=": 30, ">=": 30, "+": 40, "-": 40, "*": 50, "/": 50 }[operator] ?? -1
  }
}

export function parse(source) {
  return new Parser(source).parse()
}

let typeSerial = 0

function variableType(level) {
  return { tag: "variable", id: typeSerial++, level, link: null }
}

function primitiveType(name) {
  return { tag: "primitive", name }
}

function functionType(parameter, result) {
  return { tag: "function", parameter, result }
}

function prune(type) {
  if (type.tag === "variable" && type.link) {
    type.link = prune(type.link)
    return type.link
  }
  return type
}

function occurs(variable, type) {
  const resolved = prune(type)
  if (resolved === variable) return true
  if (resolved.tag === "function") return occurs(variable, resolved.parameter) || occurs(variable, resolved.result)
  return false
}

function lowerLevel(level, type) {
  const resolved = prune(type)
  if (resolved.tag === "variable" && !resolved.link) resolved.level = Math.min(resolved.level, level)
  if (resolved.tag === "function") {
    lowerLevel(level, resolved.parameter)
    lowerLevel(level, resolved.result)
  }
}

function unify(left, right, node) {
  const a = prune(left)
  const b = prune(right)
  if (a === b) return
  if (a.tag === "variable") {
    if (occurs(a, b)) throw new LambError("this expression creates an infinite type", node.start, node.end)
    lowerLevel(a.level, b)
    a.link = b
    return
  }
  if (b.tag === "variable") return unify(b, a, node)
  if (a.tag === "primitive" && b.tag === "primitive" && a.name === b.name) return
  if (a.tag === "function" && b.tag === "function") {
    unify(a.parameter, b.parameter, node)
    unify(a.result, b.result, node)
    return
  }
  throw new LambError(`type ${formatType(a)} is incompatible with ${formatType(b)}`, node.start, node.end)
}

function collect(type, level, found = new Set()) {
  const resolved = prune(type)
  if (resolved.tag === "variable" && !resolved.link && resolved.level > level) found.add(resolved)
  if (resolved.tag === "function") {
    collect(resolved.parameter, level, found)
    collect(resolved.result, level, found)
  }
  return found
}

function instantiate(scheme, level) {
  const replacements = new Map()
  for (const variable of scheme.variables) replacements.set(variable, variableType(level))
  const copy = type => {
    const resolved = prune(type)
    if (replacements.has(resolved)) return replacements.get(resolved)
    if (resolved.tag === "function") return functionType(copy(resolved.parameter), copy(resolved.result))
    return resolved
  }
  return copy(scheme.type)
}

function inferNode(node, environment, level) {
  const integer = primitiveType("int")
  const boolean = primitiveType("bool")
  const unit = primitiveType("unit")
  let type
  if (node.kind === "integer") type = integer
  if (node.kind === "boolean") type = boolean
  if (node.kind === "unit") type = unit
  if (node.kind === "variable") {
    const scheme = environment.get(node.name)
    if (!scheme) throw new LambError(`unbound value ${node.name}`, node.start, node.end)
    type = instantiate(scheme, level)
  }
  if (node.kind === "unary") {
    const value = inferNode(node.value, environment, level)
    unify(value, integer, node.value)
    type = integer
  }
  if (node.kind === "binary") {
    const left = inferNode(node.left, environment, level)
    const right = inferNode(node.right, environment, level)
    if (["+", "-", "*", "/"].includes(node.operator)) {
      unify(left, integer, node.left)
      unify(right, integer, node.right)
      type = integer
    } else if (["<", ">", "<=", ">="].includes(node.operator)) {
      unify(left, integer, node.left)
      unify(right, integer, node.right)
      type = boolean
    } else if (["&&", "||"].includes(node.operator)) {
      unify(left, boolean, node.left)
      unify(right, boolean, node.right)
      type = boolean
    } else {
      unify(left, right, node)
      type = boolean
    }
  }
  if (node.kind === "if") {
    const condition = inferNode(node.condition, environment, level)
    const consequent = inferNode(node.consequent, environment, level)
    const alternate = inferNode(node.alternate, environment, level)
    unify(condition, boolean, node.condition)
    unify(consequent, alternate, node)
    type = consequent
  }
  if (node.kind === "function") {
    const parameter = variableType(level + 1)
    const nested = new Map(environment)
    nested.set(node.parameter, { variables: [], type: parameter })
    const result = inferNode(node.body, nested, level + 1)
    type = functionType(parameter, result)
  }
  if (node.kind === "apply") {
    const callee = inferNode(node.callee, environment, level)
    const argument = inferNode(node.argument, environment, level)
    const result = variableType(level)
    unify(callee, functionType(argument, result), node.callee)
    type = result
  }
  if (node.kind === "let") {
    const nested = new Map(environment)
    let value
    if (node.recursive) {
      const provisional = variableType(level + 1)
      nested.set(node.name, { variables: [], type: provisional })
      value = inferNode(node.value, nested, level + 1)
      unify(provisional, value, node.value)
    } else {
      value = inferNode(node.value, environment, level + 1)
    }
    nested.set(node.name, { variables: [...collect(value, level)], type: value })
    type = inferNode(node.body, nested, level)
  }
  node.type = prune(type)
  return node.type
}

export function infer(ast) {
  typeSerial = 0
  inferNode(ast, new Map(), 0)
  return ast
}

export function formatType(type, names = new Map()) {
  const resolved = prune(type)
  if (resolved.tag === "primitive") return resolved.name
  if (resolved.tag === "variable") {
    if (!names.has(resolved)) {
      const index = names.size
      names.set(resolved, `'${String.fromCharCode(97 + index % 26)}${index > 25 ? Math.floor(index / 26) : ""}`)
    }
    return names.get(resolved)
  }
  const parameter = prune(resolved.parameter)
  const left = parameter.tag === "function" ? `(${formatType(parameter, names)})` : formatType(parameter, names)
  return `${left} -> ${formatType(resolved.result, names)}`
}

function indent(text, depth = 1) {
  const padding = "  ".repeat(depth)
  return text.split("\n").map(line => `${padding}${line}`).join("\n")
}

function typed(node) {
  const type = formatType(node.type)
  if (node.kind === "integer") return `Texp_constant Int ${node.value} : ${type}`
  if (node.kind === "boolean") return `Texp_construct ${node.value ? "true" : "false"} : ${type}`
  if (node.kind === "unit") return `Texp_construct () : ${type}`
  if (node.kind === "variable") return `Texp_ident ${node.name} : ${type}`
  if (node.kind === "unary") return `Texp_apply (~${node.operator}) : ${type}\n${indent(typed(node.value))}`
  if (node.kind === "binary") return `Texp_apply (${node.operator}) : ${type}\n${indent(typed(node.left))}\n${indent(typed(node.right))}`
  if (node.kind === "if") return `Texp_ifthenelse : ${type}\n${indent("condition\n" + indent(typed(node.condition)))}\n${indent("then\n" + indent(typed(node.consequent)))}\n${indent("else\n" + indent(typed(node.alternate)))}`
  if (node.kind === "function") return `Texp_function ${node.parameter} : ${type}\n${indent(typed(node.body))}`
  if (node.kind === "apply") return `Texp_apply : ${type}\n${indent(typed(node.callee))}\n${indent(typed(node.argument))}`
  if (node.kind === "let") return `Texp_let ${node.recursive ? "Recursive" : "Nonrecursive"} ${node.name} : ${type}\n${indent("binding\n" + indent(typed(node.value)))}\n${indent("body\n" + indent(typed(node.body)))}`
  return "Texp_unknown"
}

function lambda(node) {
  if (node.kind === "integer") return `(Lconst (Const_base (Const_int ${node.value})))`
  if (node.kind === "boolean") return `(Lconst (Const_pointer ${node.value ? 1 : 0}))`
  if (node.kind === "unit") return `(Lconst (Const_pointer 0))`
  if (node.kind === "variable") return `(Lvar ${node.name})`
  if (node.kind === "unary") return `(Lprim Pnegint\n${indent(lambda(node.value))})`
  if (node.kind === "binary") return `(Lprim ${primitiveName(node.operator)}\n${indent(lambda(node.left))}\n${indent(lambda(node.right))})`
  if (node.kind === "if") return `(Lifthenelse\n${indent(lambda(node.condition))}\n${indent(lambda(node.consequent))}\n${indent(lambda(node.alternate))})`
  if (node.kind === "function") return `(Lfunction ${node.parameter}\n${indent(lambda(node.body))})`
  if (node.kind === "apply") return `(Lapply\n${indent(lambda(node.callee))}\n${indent(lambda(node.argument))})`
  if (node.kind === "let") return `(${node.recursive ? "Lletrec" : "Llet"} ${node.name}\n${indent(lambda(node.value))}\n${indent(lambda(node.body))})`
  return "Lunknown"
}

function primitiveName(operator) {
  return { "+": "Paddint", "-": "Psubint", "*": "Pmulint", "/": "Pdivint", "=": "Pintcomp Ceq", "<>": "Pintcomp Cne", "<": "Pintcomp Clt", ">": "Pintcomp Cgt", "<=": "Pintcomp Cle", ">=": "Pintcomp Cge", "&&": "Psequand", "||": "Psequor" }[operator]
}

function freeVariables(node, bound = new Set(), found = new Set()) {
  if (node.kind === "variable" && !bound.has(node.name)) found.add(node.name)
  if (node.kind === "unary") freeVariables(node.value, bound, found)
  if (node.kind === "binary") {
    freeVariables(node.left, bound, found)
    freeVariables(node.right, bound, found)
  }
  if (node.kind === "if") {
    freeVariables(node.condition, bound, found)
    freeVariables(node.consequent, bound, found)
    freeVariables(node.alternate, bound, found)
  }
  if (node.kind === "function") freeVariables(node.body, new Set([...bound, node.parameter]), found)
  if (node.kind === "apply") {
    freeVariables(node.callee, bound, found)
    freeVariables(node.argument, bound, found)
  }
  if (node.kind === "let") {
    freeVariables(node.value, node.recursive ? new Set([...bound, node.name]) : bound, found)
    freeVariables(node.body, new Set([...bound, node.name]), found)
  }
  return found
}

function clambda(node) {
  if (node.kind === "integer") return `(Uconst (Uconst_int ${node.value}))`
  if (node.kind === "boolean") return `(Uconst (Uconst_ptr ${node.value ? 1 : 0}))`
  if (node.kind === "unit") return `(Uconst (Uconst_ptr 0))`
  if (node.kind === "variable") return `(Uvar ${node.name})`
  if (node.kind === "unary") return `(Uprim Pnegint\n${indent(clambda(node.value))})`
  if (node.kind === "binary") return `(Uprim ${primitiveName(node.operator)}\n${indent(clambda(node.left))}\n${indent(clambda(node.right))})`
  if (node.kind === "if") return `(Uifthenelse\n${indent(clambda(node.condition))}\n${indent(clambda(node.consequent))}\n${indent(clambda(node.alternate))})`
  if (node.kind === "function") {
    const captured = [...freeVariables(node.body, new Set([node.parameter]))]
    return `(Uclosure\n${indent(`function ${node.parameter} arity 1`)}\n${indent(`environment [${captured.join(", ") || "empty"}]`)}\n${indent(clambda(node.body))})`
  }
  if (node.kind === "apply") return `(Ugenericapply\n${indent(clambda(node.callee))}\n${indent(clambda(node.argument))})`
  if (node.kind === "let") return `(${node.recursive ? "Uletrec" : "Ulet"} ${node.name}\n${indent(clambda(node.value))}\n${indent(clambda(node.body))})`
  return "Uunknown"
}

function valueName(context) {
  context.serial += 1
  return `v${context.serial}`
}

function cmmNode(node, context, lines) {
  if (node.kind === "integer") {
    const name = valueName(context)
    lines.push(`${name} = ${encodeInteger(node.value)}L`)
    return name
  }
  if (node.kind === "boolean") {
    const name = valueName(context)
    lines.push(`${name} = ${node.value ? 3 : 1}L`)
    return name
  }
  if (node.kind === "unit") {
    const name = valueName(context)
    lines.push(`${name} = 1L`)
    return name
  }
  if (node.kind === "variable") return node.name
  if (node.kind === "unary") {
    const value = cmmNode(node.value, context, lines)
    const name = valueName(context)
    lines.push(`${name} = 2L - ${value}`)
    return name
  }
  if (node.kind === "binary") {
    const left = cmmNode(node.left, context, lines)
    const right = cmmNode(node.right, context, lines)
    const name = valueName(context)
    const operation = {
      "+": `${left} + ${right} - 1L`,
      "-": `${left} - ${right} + 1L`,
      "*": `tag_int(untag_int(${left}) * untag_int(${right}))`,
      "/": `tag_int(untag_int(${left}) / untag_int(${right}))`,
      "=": `tag_bool(${left} == ${right})`,
      "<>": `tag_bool(${left} != ${right})`,
      "<": `tag_bool(${left} < ${right})`,
      ">": `tag_bool(${left} > ${right})`,
      "<=": `tag_bool(${left} <= ${right})`,
      ">=": `tag_bool(${left} >= ${right})`,
      "&&": `branch_and(${left}, ${right})`,
      "||": `branch_or(${left}, ${right})`
    }[node.operator]
    lines.push(`${name} = ${operation}`)
    return name
  }
  if (node.kind === "if") {
    const condition = cmmNode(node.condition, context, lines)
    const consequentLines = []
    const alternateLines = []
    const consequent = cmmNode(node.consequent, context, consequentLines)
    const alternate = cmmNode(node.alternate, context, alternateLines)
    const name = valueName(context)
    lines.push(`${name} = if ${condition} != 1L then {`)
    lines.push(...consequentLines.map(line => `  ${line}`))
    lines.push(`  return ${consequent}`)
    lines.push("} else {")
    lines.push(...alternateLines.map(line => `  ${line}`))
    lines.push(`  return ${alternate}`)
    lines.push("}")
    return name
  }
  if (node.kind === "function") {
    const name = valueName(context)
    const captured = [...freeVariables(node.body, new Set([node.parameter]))]
    lines.push(`${name} = alloc_closure(code_${name}, [${captured.join(", ")}])`)
    const bodyLines = []
    const result = cmmNode(node.body, context, bodyLines)
    context.functions.push(`function code_${name}(${node.parameter}, env) {\n${indent(bodyLines.join("\n"))}\n  return ${result}\n}`)
    return name
  }
  if (node.kind === "apply") {
    const callee = cmmNode(node.callee, context, lines)
    const argument = cmmNode(node.argument, context, lines)
    const name = valueName(context)
    lines.push(`${name} = call_closure(${callee}, ${argument})`)
    return name
  }
  if (node.kind === "let") {
    const value = cmmNode(node.value, context, lines)
    lines.push(`${node.name} = ${value}`)
    return cmmNode(node.body, context, lines)
  }
  return "unknown"
}

function cmm(ast) {
  const context = { serial: 0, functions: [] }
  const lines = []
  const result = cmmNode(ast, context, lines)
  return [`function camlLamb__entry() {`, indent(lines.join("\n")), `  return ${result}`, `}`, ...context.functions].join("\n")
}

function assembly(ast) {
  const constants = []
  const operators = []
  const walk = node => {
    if (node.kind === "integer") constants.push(node.value)
    if (node.kind === "binary") operators.push(node.operator)
    for (const key of ["value", "left", "right", "condition", "consequent", "alternate", "body", "callee", "argument"]) {
      if (node[key] && typeof node[key] === "object") walk(node[key])
    }
  }
  walk(ast)
  const lines = ["camlLamb__entry:", "  push rbp", "  mov rbp, rsp"]
  constants.forEach((constant, index) => lines.push(`  mov r${index % 2 === 0 ? "ax" : "bx"}, ${encodeInteger(constant)}`))
  operators.forEach(operator => {
    if (operator === "+") lines.push("  lea rax, [rax + rbx - 1]")
    else if (operator === "-") lines.push("  sub rax, rbx", "  add rax, 1")
    else if (operator === "*") lines.push("  sar rax, 1", "  sar rbx, 1", "  imul rax, rbx", "  lea rax, [rax * 2 + 1]")
    else if (operator === "/") lines.push("  sar rax, 1", "  sar rbx, 1", "  cqo", "  idiv rbx", "  lea rax, [rax * 2 + 1]")
    else if (["=", "<>", "<", ">", "<=", ">="].includes(operator)) lines.push("  cmp rax, rbx", "  setcc al", "  movzx rax, al", "  lea rax, [rax * 2 + 1]")
    else lines.push(`  call camlLamb__${operator === "&&" ? "and" : "or"}`)
  })
  if (constants.length === 0) lines.push("  mov rax, 1")
  lines.push("  pop rbp", "  ret")
  return lines.join("\n")
}

export function encodeInteger(value) {
  return (BigInt(value) << 1n) | 1n
}

export function decodeInteger(value) {
  const word = BigInt(value)
  if ((word & 1n) === 0n) throw new LambError("the low bit is zero so this word is pointer-shaped")
  return word >> 1n
}

export function wordBits(value, width = 64) {
  const mask = (1n << BigInt(width)) - 1n
  return (BigInt(value) & mask).toString(2).padStart(width, "0")
}

function countNodes(node) {
  let count = 1
  for (const key of ["value", "left", "right", "condition", "consequent", "alternate", "body", "callee", "argument"]) {
    if (node[key] && typeof node[key] === "object") count += countNodes(node[key])
  }
  return count
}

export const stages = [
  { key: "source", name: "Source", phase: "surface syntax", note: "The parser recognizes a focused expression subset with nested comments and OCaml precedence." },
  { key: "typedtree", name: "Typedtree", phase: "type checking", note: "Hindley–Milner inference annotates every expression and generalizes nonrecursive let bindings." },
  { key: "lambda", name: "Lambda", phase: "language lowering", note: "Typed syntax becomes a smaller functional intermediate form with explicit primitives and applications." },
  { key: "clambda", name: "Clambda", phase: "closure conversion", note: "Functions expose captured environments while generic applications make closure calling conventions visible." },
  { key: "cmm", name: "Cmm", phase: "machine lowering", note: "Tagged arithmetic and closure allocation become explicit in a machine-oriented control representation." },
  { key: "assembly", name: "Assembly", phase: "instruction selection", note: "A compact x86-64 sketch demonstrates tagged arithmetic after instruction selection." }
]

export function compile(source) {
  const ast = infer(parse(source))
  const output = {
    source: source.trim(),
    typedtree: typed(ast),
    lambda: lambda(ast),
    clambda: clambda(ast),
    cmm: cmm(ast),
    assembly: assembly(ast)
  }
  return {
    ast,
    type: formatType(ast.type),
    nodes: countNodes(ast),
    stages: stages.map(stage => ({ ...stage, output: output[stage.key], lines: output[stage.key].split("\n").length }))
  }
}

export const samples = [
  {
    name: "tagged arithmetic",
    source: "let x = 21 in\nlet y = x * 2 in\ny + 1"
  },
  {
    name: "closure capture",
    source: "let offset = 7 in\nlet add = fun value -> value + offset in\nadd 35"
  },
  {
    name: "polymorphic identity",
    source: "let identity = fun value -> value in\nlet number = identity 42 in\nidentity true"
  },
  {
    name: "recursive function",
    source: "let rec sum n =\n  if n = 0 then 0\n  else n + sum (n - 1)\nin\nsum 6"
  }
]
