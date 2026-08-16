import { LambError } from "../syntax/lexer.js"
import { arrow, formatType, generalize, instantiate, list, option, primitive, prune, resetTypes, tuple, unify, variable } from "./system.js"

const int = primitive("int")
const float = primitive("float")
const bool = primitive("bool")
const string = primitive("string")
const char = primitive("char")
const unit = primitive("unit")

function constructors(level) {
  const optional = variable(level)
  return new Map([
    ["None", { variables: [optional], type: option(optional) }],
    ["Some", { variables: [optional], type: arrow(optional, option(optional)) }]
  ])
}

function bindPattern(pattern, expected, environment, level, names = new Set()) {
  pattern.type = prune(expected)
  if (pattern.kind === "pwildcard") return
  if (pattern.kind === "pvariable") {
    if (names.has(pattern.name)) throw new LambError(`variable ${pattern.name} is bound several times in this pattern`, pattern.start, pattern.end, "E_PATTERN_BINDING")
    names.add(pattern.name)
    environment.set(pattern.name, { variables: [], type: expected })
    return
  }
  if (pattern.kind === "pinteger") return unify(expected, int, pattern)
  if (pattern.kind === "pboolean") return unify(expected, bool, pattern)
  if (pattern.kind === "pstring") return unify(expected, string, pattern)
  if (pattern.kind === "pcharacter") return unify(expected, char, pattern)
  if (pattern.kind === "punit") return unify(expected, unit, pattern)
  if (pattern.kind === "pnil") return unify(expected, list(variable(level)), pattern)
  if (pattern.kind === "pcons") {
    const item = variable(level)
    unify(expected, list(item), pattern)
    bindPattern(pattern.head, item, environment, level, names)
    bindPattern(pattern.tail, list(item), environment, level, names)
    return
  }
  if (pattern.kind === "ptuple") {
    const items = pattern.items.map(() => variable(level))
    unify(expected, tuple(items), pattern)
    pattern.items.forEach((entry, index) => bindPattern(entry, items[index], environment, level, names))
    return
  }
  if (pattern.kind === "pconstructor") {
    if (pattern.name === "None") {
      if (pattern.argument) throw new LambError("constructor None expects no argument", pattern.start, pattern.end, "E_CONSTRUCTOR_ARITY")
      return unify(expected, option(variable(level)), pattern)
    }
    if (pattern.name === "Some") {
      if (!pattern.argument) throw new LambError("constructor Some expects one argument", pattern.start, pattern.end, "E_CONSTRUCTOR_ARITY")
      const item = variable(level)
      unify(expected, option(item), pattern)
      bindPattern(pattern.argument, item, environment, level, names)
      return
    }
    throw new LambError(`unknown constructor ${pattern.name}`, pattern.start, pattern.end, "E_CONSTRUCTOR")
  }
}

function inferNode(node, environment, level, constructorEnvironment) {
  let type
  if (node.kind === "integer") type = int
  else if (node.kind === "float") type = float
  else if (node.kind === "boolean") type = bool
  else if (node.kind === "string") type = string
  else if (node.kind === "character") type = char
  else if (node.kind === "unit") type = unit
  else if (node.kind === "variable") {
    const scheme = environment.get(node.name)
    if (!scheme) throw new LambError(`unbound value ${node.name}`, node.start, node.end, "E_UNBOUND_VALUE")
    type = instantiate(scheme, level)
  } else if (node.kind === "constructor") {
    const scheme = constructorEnvironment.get(node.name)
    if (!scheme) throw new LambError(`unknown constructor ${node.name}`, node.start, node.end, "E_CONSTRUCTOR")
    type = instantiate(scheme, level)
  } else if (node.kind === "unary") {
    const value = inferNode(node.value, environment, level, constructorEnvironment)
    unify(value, int, node.value)
    type = int
  } else if (node.kind === "binary") {
    const left = inferNode(node.left, environment, level, constructorEnvironment)
    const right = inferNode(node.right, environment, level, constructorEnvironment)
    if (["+", "-", "*", "/", "mod"].includes(node.operator)) {
      unify(left, int, node.left)
      unify(right, int, node.right)
      type = int
    } else if (node.operator === "^") {
      unify(left, string, node.left)
      unify(right, string, node.right)
      type = string
    } else if (["<", ">", "<=", ">="].includes(node.operator)) {
      unify(left, right, node)
      const resolved = prune(left)
      if (resolved.tag === "primitive" && !["int", "float", "string", "char"].includes(resolved.name)) throw new LambError(`operator ${node.operator} does not order ${formatType(resolved)}`, node.start, node.end, "E_OPERATOR")
      type = bool
    } else if (["&&", "||"].includes(node.operator)) {
      unify(left, bool, node.left)
      unify(right, bool, node.right)
      type = bool
    } else {
      unify(left, right, node)
      type = bool
    }
  } else if (node.kind === "cons") {
    const head = inferNode(node.head, environment, level, constructorEnvironment)
    const tail = inferNode(node.tail, environment, level, constructorEnvironment)
    unify(tail, list(head), node.tail)
    type = tail
  } else if (node.kind === "list") {
    const item = variable(level)
    for (const entry of node.items) unify(inferNode(entry, environment, level, constructorEnvironment), item, entry)
    type = list(item)
  } else if (node.kind === "tuple") type = tuple(node.items.map(item => inferNode(item, environment, level, constructorEnvironment)))
  else if (node.kind === "sequence") {
    const first = inferNode(node.first, environment, level, constructorEnvironment)
    unify(first, unit, node.first)
    type = inferNode(node.second, environment, level, constructorEnvironment)
  } else if (node.kind === "if") {
    const condition = inferNode(node.condition, environment, level, constructorEnvironment)
    const consequent = inferNode(node.consequent, environment, level, constructorEnvironment)
    const alternate = inferNode(node.alternate, environment, level, constructorEnvironment)
    unify(condition, bool, node.condition)
    unify(consequent, alternate, node)
    type = consequent
  } else if (node.kind === "function") {
    const parameter = variable(level + 1)
    const nested = new Map(environment)
    bindPattern(node.parameter, parameter, nested, level + 1)
    const result = inferNode(node.body, nested, level + 1, constructorEnvironment)
    type = arrow(parameter, result)
  } else if (node.kind === "apply") {
    const callee = inferNode(node.callee, environment, level, constructorEnvironment)
    const argument = inferNode(node.argument, environment, level, constructorEnvironment)
    const result = variable(level)
    unify(callee, arrow(argument, result), node.callee)
    type = result
  } else if (node.kind === "let") {
    const nested = new Map(environment)
    let value
    if (node.recursive) {
      const provisional = variable(level + 1)
      nested.set(node.name, { variables: [], type: provisional })
      value = inferNode(node.value, nested, level + 1, constructorEnvironment)
      unify(provisional, value, node.value)
    } else value = inferNode(node.value, environment, level + 1, constructorEnvironment)
    nested.set(node.name, generalize(value, level))
    type = inferNode(node.body, nested, level, constructorEnvironment)
  } else if (node.kind === "match") {
    const target = inferNode(node.target, environment, level, constructorEnvironment)
    const result = variable(level)
    for (const entry of node.cases) {
      const nested = new Map(environment)
      bindPattern(entry.pattern, target, nested, level)
      unify(inferNode(entry.body, nested, level, constructorEnvironment), result, entry.body)
    }
    type = result
  } else throw new LambError(`unsupported expression ${node.kind}`, node.start, node.end, "E_INTERNAL")
  node.type = prune(type)
  return node.type
}

export function infer(ast) {
  resetTypes()
  inferNode(ast, new Map(), 0, constructors(0))
  return ast
}

export { formatType }
