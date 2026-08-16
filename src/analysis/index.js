function children(node) {
  if (!node || typeof node !== "object") return []
  const direct = [node.value, node.left, node.right, node.head, node.tail, node.first, node.second, node.condition, node.consequent, node.alternate, node.body, node.callee, node.argument, node.target]
  const arrays = [node.items, node.cases?.flatMap(entry => [entry.pattern, entry.body])]
  return [...direct, ...arrays.flatMap(entry => entry || [])].filter(entry => entry && typeof entry === "object")
}

export function walk(node, visit, depth = 0) {
  visit(node, depth)
  for (const child of children(node)) walk(child, visit, depth + 1)
}

function patternNames(pattern, names = new Set()) {
  if (pattern.kind === "pvariable") names.add(pattern.name)
  for (const child of children(pattern)) patternNames(child, names)
  return names
}

export function freeVariables(node, bound = new Set(), found = new Set()) {
  if (node.kind === "variable" && !bound.has(node.name)) found.add(node.name)
  if (node.kind === "function") {
    freeVariables(node.body, new Set([...bound, ...patternNames(node.parameter)]), found)
    return found
  }
  if (node.kind === "let") {
    freeVariables(node.value, node.recursive ? new Set([...bound, node.name]) : bound, found)
    freeVariables(node.body, new Set([...bound, node.name]), found)
    return found
  }
  if (node.kind === "match") {
    freeVariables(node.target, bound, found)
    for (const entry of node.cases) freeVariables(entry.body, new Set([...bound, ...patternNames(entry.pattern)]), found)
    return found
  }
  for (const child of children(node)) freeVariables(child, bound, found)
  return found
}

function coverage(cases) {
  const patterns = cases.map(entry => entry.pattern)
  const total = patterns.some(pattern => pattern.kind === "pwildcard" || pattern.kind === "pvariable")
  const booleans = new Set(patterns.filter(pattern => pattern.kind === "pboolean").map(pattern => pattern.value))
  const lists = new Set(patterns.map(pattern => pattern.kind))
  const options = new Set(patterns.filter(pattern => pattern.kind === "pconstructor").map(pattern => pattern.name))
  return total || booleans.size === 2 || lists.has("pnil") && lists.has("pcons") || options.has("None") && options.has("Some")
}

export function analyze(ast) {
  const metrics = { nodes: 0, depth: 0, functions: 0, applications: 0, matches: 0, allocations: 0, recursive: 0 }
  const warnings = []
  walk(ast, (node, depth) => {
    metrics.nodes += 1
    metrics.depth = Math.max(metrics.depth, depth)
    if (node.kind === "function") metrics.functions += 1
    if (node.kind === "apply") metrics.applications += 1
    if (["tuple", "list", "cons", "constructor", "function"].includes(node.kind)) metrics.allocations += 1
    if (node.kind === "let" && node.recursive) metrics.recursive += 1
    if (node.kind === "match") {
      metrics.matches += 1
      if (!coverage(node.cases)) warnings.push({ code: "W_NONEXHAUSTIVE", message: "this pattern matching is not exhaustive", start: node.start, end: node.end })
      const catchall = node.cases.findIndex(entry => ["pwildcard", "pvariable"].includes(entry.pattern.kind))
      if (catchall >= 0 && catchall < node.cases.length - 1) warnings.push({ code: "W_REDUNDANT_CASE", message: "a pattern after the catch-all case is unreachable", start: node.cases[catchall + 1].start, end: node.cases.at(-1).end })
    }
  })
  return { metrics, warnings, free: [...freeVariables(ast)].sort() }
}
