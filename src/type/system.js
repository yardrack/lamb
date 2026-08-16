import { LambError } from "../syntax/lexer.js"

let serial = 0

export const primitive = name => ({ tag: "primitive", name })
export const variable = level => ({ tag: "variable", id: serial++, level, link: null })
export const arrow = (parameter, result) => ({ tag: "function", parameter, result })
export const tuple = items => ({ tag: "tuple", items })
export const list = item => ({ tag: "list", item })
export const option = item => ({ tag: "option", item })

export function resetTypes() {
  serial = 0
}

export function prune(type) {
  if (type.tag === "variable" && type.link) {
    type.link = prune(type.link)
    return type.link
  }
  return type
}

function children(type) {
  const resolved = prune(type)
  if (resolved.tag === "function") return [resolved.parameter, resolved.result]
  if (resolved.tag === "tuple") return resolved.items
  if (resolved.tag === "list" || resolved.tag === "option") return [resolved.item]
  return []
}

function occurs(target, type) {
  const resolved = prune(type)
  return resolved === target || children(resolved).some(child => occurs(target, child))
}

function lower(level, type) {
  const resolved = prune(type)
  if (resolved.tag === "variable" && !resolved.link) resolved.level = Math.min(resolved.level, level)
  for (const child of children(resolved)) lower(level, child)
}

export function unify(left, right, node) {
  const a = prune(left)
  const b = prune(right)
  if (a === b) return
  if (a.tag === "variable") {
    if (occurs(a, b)) throw new LambError("this expression creates an infinite type", node.start, node.end, "E_TYPE_OCCURS")
    lower(a.level, b)
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
  if ((a.tag === "list" && b.tag === "list") || (a.tag === "option" && b.tag === "option")) {
    unify(a.item, b.item, node)
    return
  }
  if (a.tag === "tuple" && b.tag === "tuple" && a.items.length === b.items.length) {
    a.items.forEach((item, index) => unify(item, b.items[index], node))
    return
  }
  throw new LambError(`type ${formatType(a)} is incompatible with ${formatType(b)}`, node.start, node.end, "E_TYPE_MISMATCH")
}

function collect(type, level, found = new Set()) {
  const resolved = prune(type)
  if (resolved.tag === "variable" && !resolved.link && resolved.level > level) found.add(resolved)
  for (const child of children(resolved)) collect(child, level, found)
  return found
}

export function generalize(type, level) {
  return { variables: [...collect(type, level)], type }
}

export function instantiate(scheme, level) {
  const replacements = new Map(scheme.variables.map(entry => [entry, variable(level)]))
  const copy = type => {
    const resolved = prune(type)
    if (replacements.has(resolved)) return replacements.get(resolved)
    if (resolved.tag === "function") return arrow(copy(resolved.parameter), copy(resolved.result))
    if (resolved.tag === "tuple") return tuple(resolved.items.map(copy))
    if (resolved.tag === "list") return list(copy(resolved.item))
    if (resolved.tag === "option") return option(copy(resolved.item))
    return resolved
  }
  return copy(scheme.type)
}

export function formatType(type, names = new Map()) {
  const resolved = prune(type)
  if (resolved.tag === "primitive") return resolved.name
  if (resolved.tag === "variable") {
    if (!names.has(resolved)) {
      const index = names.size
      const suffix = index > 25 ? Math.floor(index / 26) : ""
      names.set(resolved, `'${String.fromCharCode(97 + index % 26)}${suffix}`)
    }
    return names.get(resolved)
  }
  if (resolved.tag === "function") {
    const parameter = prune(resolved.parameter)
    const rendered = formatType(parameter, names)
    return `${parameter.tag === "function" ? `(${rendered})` : rendered} -> ${formatType(resolved.result, names)}`
  }
  if (resolved.tag === "tuple") return resolved.items.map(item => formatType(item, names)).join(" * ")
  if (resolved.tag === "list") {
    const item = prune(resolved.item)
    const rendered = formatType(item, names)
    return `${item.tag === "function" || item.tag === "tuple" ? `(${rendered})` : rendered} list`
  }
  if (resolved.tag === "option") {
    const item = prune(resolved.item)
    const rendered = formatType(item, names)
    return `${item.tag === "function" || item.tag === "tuple" ? `(${rendered})` : rendered} option`
  }
  return "unknown"
}
