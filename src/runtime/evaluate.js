import { LambError } from "../syntax/lexer.js"

const value = (kind, data = {}) => ({ kind, ...data })

function match(pattern, target, bindings = new Map()) {
  if (pattern.kind === "pwildcard") return bindings
  if (pattern.kind === "pvariable") return new Map([...bindings, [pattern.name, target]])
  if (pattern.kind === "pinteger" && target.kind === "integer" && pattern.value === target.value) return bindings
  if (pattern.kind === "pboolean" && target.kind === "boolean" && pattern.value === target.value) return bindings
  if (pattern.kind === "pstring" && target.kind === "string" && pattern.value === target.value) return bindings
  if (pattern.kind === "pcharacter" && target.kind === "character" && pattern.value === target.value) return bindings
  if (pattern.kind === "punit" && target.kind === "unit") return bindings
  if (pattern.kind === "pnil" && target.kind === "list" && target.items.length === 0) return bindings
  if (pattern.kind === "pcons" && target.kind === "list" && target.items.length) {
    const head = match(pattern.head, target.items[0], bindings)
    return head && match(pattern.tail, value("list", { items: target.items.slice(1) }), head)
  }
  if (pattern.kind === "ptuple" && target.kind === "tuple" && pattern.items.length === target.items.length) {
    return pattern.items.reduce((result, entry, index) => result && match(entry, target.items[index], result), bindings)
  }
  if (pattern.kind === "pconstructor" && target.kind === "constructor" && pattern.name === target.name) {
    if (!pattern.argument) return target.argument === null ? bindings : null
    return target.argument === null ? null : match(pattern.argument, target.argument, bindings)
  }
  return null
}

function scalar(entry) {
  if (["integer", "float", "boolean", "string", "character"].includes(entry.kind)) return entry.value
  throw new Error(`cannot compare ${entry.kind} values`)
}

function binary(operator, left, right) {
  if (operator === "+") return value("integer", { value: left.value + right.value })
  if (operator === "-") return value("integer", { value: left.value - right.value })
  if (operator === "*") return value("integer", { value: left.value * right.value })
  if (operator === "/") return value("integer", { value: left.value / right.value })
  if (operator === "mod") return value("integer", { value: left.value % right.value })
  if (operator === "^") return value("string", { value: left.value + right.value })
  if (operator === "&&") return value("boolean", { value: left.value && right.value })
  if (operator === "||") return value("boolean", { value: left.value || right.value })
  if (operator === "=") return value("boolean", { value: formatValue(left) === formatValue(right) })
  if (operator === "<>") return value("boolean", { value: formatValue(left) !== formatValue(right) })
  const a = scalar(left)
  const b = scalar(right)
  return value("boolean", { value: operator === "<" ? a < b : operator === ">" ? a > b : operator === "<=" ? a <= b : a >= b })
}

function apply(callee, argument, node) {
  if (callee.kind === "constructor") return value("constructor", { name: callee.name, argument })
  if (callee.kind !== "closure") throw new LambError("this expression is not a function", node.start, node.end, "E_RUNTIME_APPLY")
  const bindings = match(callee.parameter, argument)
  if (!bindings) throw new LambError("function argument did not match its parameter pattern", node.start, node.end, "E_MATCH_FAILURE")
  return evaluateNode(callee.body, new Map([...callee.environment, ...bindings]))
}

function evaluateNode(node, environment) {
  if (["integer", "float", "boolean", "string", "character"].includes(node.kind)) return value(node.kind, { value: node.value })
  if (node.kind === "unit") return value("unit")
  if (node.kind === "variable") return environment.get(node.name)
  if (node.kind === "constructor") return node.name === "None" ? value("constructor", { name: "None", argument: null }) : value("constructor", { name: node.name, argument: null, pending: true })
  if (node.kind === "unary") return value("integer", { value: -evaluateNode(node.value, environment).value })
  if (node.kind === "binary") {
    const left = evaluateNode(node.left, environment)
    if (node.operator === "&&" && !left.value) return left
    if (node.operator === "||" && left.value) return left
    return binary(node.operator, left, evaluateNode(node.right, environment))
  }
  if (node.kind === "cons") return value("list", { items: [evaluateNode(node.head, environment), ...evaluateNode(node.tail, environment).items] })
  if (node.kind === "list") return value("list", { items: node.items.map(entry => evaluateNode(entry, environment)) })
  if (node.kind === "tuple") return value("tuple", { items: node.items.map(entry => evaluateNode(entry, environment)) })
  if (node.kind === "sequence") {
    evaluateNode(node.first, environment)
    return evaluateNode(node.second, environment)
  }
  if (node.kind === "if") return evaluateNode(node.condition, environment).value ? evaluateNode(node.consequent, environment) : evaluateNode(node.alternate, environment)
  if (node.kind === "function") return value("closure", { parameter: node.parameter, body: node.body, environment })
  if (node.kind === "apply") return apply(evaluateNode(node.callee, environment), evaluateNode(node.argument, environment), node)
  if (node.kind === "let") {
    const nested = new Map(environment)
    if (node.recursive) {
      const entry = evaluateNode(node.value, nested)
      nested.set(node.name, entry)
      if (entry.kind === "closure") entry.environment = nested
    } else nested.set(node.name, evaluateNode(node.value, environment))
    return evaluateNode(node.body, nested)
  }
  if (node.kind === "match") {
    const target = evaluateNode(node.target, environment)
    for (const entry of node.cases) {
      const bindings = match(entry.pattern, target)
      if (bindings) return evaluateNode(entry.body, new Map([...environment, ...bindings]))
    }
    throw new LambError("pattern matching failed", node.start, node.end, "E_MATCH_FAILURE")
  }
  throw new LambError(`unsupported runtime expression ${node.kind}`, node.start, node.end, "E_INTERNAL")
}

export function evaluate(ast) {
  return evaluateNode(ast, new Map())
}

export function formatValue(entry) {
  if (entry.kind === "integer") return entry.value.toString()
  if (entry.kind === "float" || entry.kind === "boolean") return String(entry.value)
  if (entry.kind === "string") return JSON.stringify(entry.value)
  if (entry.kind === "character") return `'${entry.value.replaceAll("'", "\\'")}'`
  if (entry.kind === "unit") return "()"
  if (entry.kind === "tuple") return `(${entry.items.map(formatValue).join(", ")})`
  if (entry.kind === "list") return `[${entry.items.map(formatValue).join("; ")}]`
  if (entry.kind === "constructor") return entry.argument === null ? entry.name : `${entry.name} ${formatValue(entry.argument)}`
  if (entry.kind === "closure") return "<fun>"
  return "<value>"
}
