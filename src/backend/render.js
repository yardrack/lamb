import { freeVariables } from "../analysis/index.js"
import { formatType } from "../type/infer.js"
import { encodeInteger } from "../runtime/word.js"

const indent = (text, depth = 1) => text.split("\n").map(line => `${"  ".repeat(depth)}${line}`).join("\n")

function pattern(node) {
  if (node.kind === "pwildcard") return "_"
  if (node.kind === "pvariable") return node.name
  if (node.kind === "pinteger") return node.value.toString()
  if (node.kind === "pboolean") return String(node.value)
  if (node.kind === "pstring") return JSON.stringify(node.value)
  if (node.kind === "pcharacter") return `'${node.value}'`
  if (node.kind === "punit") return "()"
  if (node.kind === "pnil") return "[]"
  if (node.kind === "pcons") return `${pattern(node.head)} :: ${pattern(node.tail)}`
  if (node.kind === "ptuple") return `(${node.items.map(pattern).join(", ")})`
  if (node.kind === "pconstructor") return node.argument ? `${node.name} ${pattern(node.argument)}` : node.name
  return "_"
}

function constant(node, prefix) {
  if (node.kind === "integer") return `${prefix}_int ${node.value}`
  if (node.kind === "float") return `${prefix}_float ${node.value}`
  if (node.kind === "string") return `${prefix}_string ${JSON.stringify(node.value)}`
  if (node.kind === "character") return `${prefix}_char ${JSON.stringify(node.value)}`
  if (node.kind === "boolean") return `${prefix}_pointer ${node.value ? 1 : 0}`
  if (node.kind === "unit") return `${prefix}_pointer 0`
  return null
}

function primitive(operator) {
  return { "+": "Paddint", "-": "Psubint", "*": "Pmulint", "/": "Pdivint", mod: "Pmodint", "^": "Pstringconcat", "=": "Pcompare Ceq", "<>": "Pcompare Cne", "<": "Pcompare Clt", ">": "Pcompare Cgt", "<=": "Pcompare Cle", ">=": "Pcompare Cge", "&&": "Psequand", "||": "Psequor" }[operator]
}

export function typedtree(node) {
  const type = formatType(node.type)
  const literal = constant(node, "Const")
  if (literal) return `Texp_constant ${literal} : ${type}`
  if (node.kind === "variable") return `Texp_ident ${node.name} : ${type}`
  if (node.kind === "constructor") return `Texp_construct ${node.name} : ${type}`
  if (node.kind === "unary") return `Texp_apply (~${node.operator}) : ${type}\n${indent(typedtree(node.value))}`
  if (node.kind === "binary") return `Texp_apply (${node.operator}) : ${type}\n${indent(typedtree(node.left))}\n${indent(typedtree(node.right))}`
  if (node.kind === "cons") return `Texp_construct (::) : ${type}\n${indent(typedtree(node.head))}\n${indent(typedtree(node.tail))}`
  if (node.kind === "list" || node.kind === "tuple") return `Texp_${node.kind} : ${type}\n${node.items.map(item => indent(typedtree(item))).join("\n")}`
  if (node.kind === "sequence") return `Texp_sequence : ${type}\n${indent(typedtree(node.first))}\n${indent(typedtree(node.second))}`
  if (node.kind === "if") return `Texp_ifthenelse : ${type}\n${indent(typedtree(node.condition))}\n${indent(typedtree(node.consequent))}\n${indent(typedtree(node.alternate))}`
  if (node.kind === "function") return `Texp_function ${pattern(node.parameter)} : ${type}\n${indent(typedtree(node.body))}`
  if (node.kind === "apply") return `Texp_apply : ${type}\n${indent(typedtree(node.callee))}\n${indent(typedtree(node.argument))}`
  if (node.kind === "let") return `Texp_let ${node.recursive ? "Recursive" : "Nonrecursive"} ${node.name} : ${type}\n${indent(typedtree(node.value))}\n${indent(typedtree(node.body))}`
  if (node.kind === "match") return `Texp_match : ${type}\n${indent(typedtree(node.target))}\n${node.cases.map(entry => indent(`Tcase ${pattern(entry.pattern)}\n${indent(typedtree(entry.body))}`)).join("\n")}`
  return `Texp_unknown : ${type}`
}

function patternNames(node, names = new Set()) {
  if (node.kind === "pvariable") names.add(node.name)
  if (node.head) patternNames(node.head, names)
  if (node.tail) patternNames(node.tail, names)
  if (node.argument) patternNames(node.argument, names)
  for (const item of node.items || []) patternNames(item, names)
  return names
}

function functional(node, family) {
  const lead = family === "lambda" ? "L" : "U"
  const literal = constant(node, family === "lambda" ? "Const" : "Uconst")
  if (literal) return `(${lead}const (${literal}))`
  if (node.kind === "variable") return `(${lead}var ${node.name})`
  if (node.kind === "constructor") return `(${lead}construct ${node.name})`
  if (node.kind === "unary") return `(${lead}prim Pnegint\n${indent(functional(node.value, family))})`
  if (node.kind === "binary") return `(${lead}prim ${primitive(node.operator)}\n${indent(functional(node.left, family))}\n${indent(functional(node.right, family))})`
  if (node.kind === "cons") return `(${lead}prim Pmakeblock Cons\n${indent(functional(node.head, family))}\n${indent(functional(node.tail, family))})`
  if (node.kind === "list") return node.items.reduceRight((tail, item) => `(${lead}prim Pmakeblock Cons\n${indent(functional(item, family))}\n${indent(tail)})`, `(${lead}const Emptylist)`)
  if (node.kind === "tuple") return `(${lead}prim Pmakeblock Tuple\n${node.items.map(item => indent(functional(item, family))).join("\n")})`
  if (node.kind === "sequence") return `(${lead}sequence\n${indent(functional(node.first, family))}\n${indent(functional(node.second, family))})`
  if (node.kind === "if") return `(${lead}ifthenelse\n${indent(functional(node.condition, family))}\n${indent(functional(node.consequent, family))}\n${indent(functional(node.alternate, family))})`
  if (node.kind === "function") {
    if (family === "lambda") return `(Lfunction ${pattern(node.parameter)}\n${indent(functional(node.body, family))})`
    const captured = [...freeVariables(node.body, new Set(patternNames(node.parameter)))]
    return `(Uclosure\n${indent(`function ${pattern(node.parameter)} arity 1`)}\n${indent(`environment [${captured.join(", ") || "empty"}]`)}\n${indent(functional(node.body, family))})`
  }
  if (node.kind === "apply") return `(${family === "lambda" ? "Lapply" : "Ugenericapply"}\n${indent(functional(node.callee, family))}\n${indent(functional(node.argument, family))})`
  if (node.kind === "let") return `(${lead}${node.recursive ? "letrec" : "let"} ${node.name}\n${indent(functional(node.value, family))}\n${indent(functional(node.body, family))})`
  if (node.kind === "match") return `(${lead}switch\n${indent(functional(node.target, family))}\n${node.cases.map(entry => indent(`case ${pattern(entry.pattern)}\n${indent(functional(entry.body, family))}`)).join("\n")})`
  return `(${lead}unknown)`
}

export const lambda = node => functional(node, "lambda")
export const clambda = node => functional(node, "clambda")

function cmmNode(node, context, lines) {
  const next = () => `v${++context.serial}`
  if (node.kind === "integer") {
    const name = next()
    lines.push(`${name} = ${encodeInteger(node.value)}L`)
    return name
  }
  if (node.kind === "boolean" || node.kind === "unit") {
    const name = next()
    lines.push(`${name} = ${node.kind === "boolean" && node.value ? 3 : 1}L`)
    return name
  }
  if (["float", "string", "character", "constructor", "tuple", "list", "cons"].includes(node.kind)) {
    const name = next()
    lines.push(`${name} = alloc_${node.kind}()`)
    return name
  }
  if (node.kind === "variable") return node.name
  if (node.kind === "unary") {
    const item = cmmNode(node.value, context, lines)
    const name = next()
    lines.push(`${name} = 2L - ${item}`)
    return name
  }
  if (node.kind === "binary") {
    const left = cmmNode(node.left, context, lines)
    const right = cmmNode(node.right, context, lines)
    const name = next()
    const operation = { "+": `${left} + ${right} - 1L`, "-": `${left} - ${right} + 1L`, "*": `tag_int(untag_int(${left}) * untag_int(${right}))`, "/": `tag_int(untag_int(${left}) / untag_int(${right}))`, mod: `tag_int(untag_int(${left}) % untag_int(${right}))`, "^": `caml_string_concat(${left}, ${right})`, "=": `tag_bool(${left} == ${right})`, "<>": `tag_bool(${left} != ${right})`, "<": `tag_bool(${left} < ${right})`, ">": `tag_bool(${left} > ${right})`, "<=": `tag_bool(${left} <= ${right})`, ">=": `tag_bool(${left} >= ${right})`, "&&": `branch_and(${left}, ${right})`, "||": `branch_or(${left}, ${right})` }[node.operator]
    lines.push(`${name} = ${operation}`)
    return name
  }
  if (node.kind === "function") {
    const name = next()
    lines.push(`${name} = alloc_closure(code_${name})`)
    return name
  }
  if (node.kind === "apply") {
    const callee = cmmNode(node.callee, context, lines)
    const argument = cmmNode(node.argument, context, lines)
    const name = next()
    lines.push(`${name} = call_closure(${callee}, ${argument})`)
    return name
  }
  if (node.kind === "let") {
    const item = cmmNode(node.value, context, lines)
    lines.push(`${node.name} = ${item}`)
    return cmmNode(node.body, context, lines)
  }
  if (node.kind === "if" || node.kind === "match" || node.kind === "sequence") {
    const operands = node.kind === "if" ? [node.condition, node.consequent, node.alternate] : node.kind === "sequence" ? [node.first, node.second] : [node.target, ...node.cases.map(entry => entry.body)]
    const values = operands.map(entry => cmmNode(entry, context, lines))
    const name = next()
    lines.push(`${name} = ${node.kind}_result(${values.join(", ")})`)
    return name
  }
  return "unknown"
}

export function cmm(ast) {
  const lines = []
  const result = cmmNode(ast, { serial: 0 }, lines)
  return `function camlLamb__entry() {\n${indent(lines.join("\n"))}\n  return ${result}\n}`
}

export function assembly(ast) {
  const constants = []
  const operators = []
  const visit = node => {
    if (!node || typeof node !== "object") return
    if (node.kind === "integer") constants.push(node.value)
    if (node.kind === "binary") operators.push(node.operator)
    for (const entry of Object.values(node)) {
      if (Array.isArray(entry)) entry.forEach(visit)
      else if (entry && typeof entry === "object" && !entry.tag) visit(entry)
    }
  }
  visit(ast)
  const lines = ["camlLamb__entry:", "  push rbp", "  mov rbp, rsp"]
  constants.forEach((entry, index) => lines.push(`  mov ${index % 2 ? "rbx" : "rax"}, ${encodeInteger(entry)}`))
  for (const operator of operators) {
    if (operator === "+") lines.push("  lea rax, [rax + rbx - 1]")
    else if (operator === "-") lines.push("  sub rax, rbx", "  add rax, 1")
    else if (["*", "/", "mod"].includes(operator)) lines.push("  sar rax, 1", `  ${operator === "*" ? "imul rax, rbx" : "idiv rbx"}`, "  lea rax, [rax * 2 + 1]")
    else lines.push(`  call camlLamb__${primitive(operator).replaceAll(" ", "_")}`)
  }
  if (!constants.length) lines.push("  mov rax, 1")
  lines.push("  pop rbp", "  ret")
  return lines.join("\n")
}
