import { LambError } from "../syntax/lexer.js"

export function encodeInteger(value) {
  return (BigInt(value) << 1n) | 1n
}

export function decodeInteger(value) {
  const word = BigInt(value)
  if ((word & 1n) === 0n) throw new LambError("the low bit is zero so this word is pointer-shaped", 0, 1, "E_POINTER_WORD")
  return word >> 1n
}

export function wordBits(value, width = 64) {
  const mask = (1n << BigInt(width)) - 1n
  return (BigInt(value) & mask).toString(2).padStart(width, "0")
}

export function representation(entry, state = { next: 4096n, blocks: [] }) {
  if (entry.kind === "integer") return { word: encodeInteger(entry.value), immediate: true, blocks: state.blocks }
  if (entry.kind === "boolean") return { word: entry.value ? 3n : 1n, immediate: true, blocks: state.blocks }
  if (entry.kind === "unit" || entry.kind === "constructor" && entry.name === "None") return { word: 1n, immediate: true, blocks: state.blocks }
  if (entry.kind === "character") return { word: (BigInt(entry.value.codePointAt(0)) << 1n) | 1n, immediate: true, blocks: state.blocks }
  const fields = entry.kind === "tuple" || entry.kind === "list" ? entry.items : entry.kind === "constructor" && entry.argument ? [entry.argument] : []
  const address = state.next
  state.next += 8n * BigInt(fields.length + 1)
  const block = { address: `0x${address.toString(16)}`, tag: entry.kind, fields: [] }
  state.blocks.push(block)
  for (const field of fields) block.fields.push(representation(field, state).word.toString())
  return { word: address, immediate: false, blocks: state.blocks }
}
