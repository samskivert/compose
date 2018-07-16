
export const enum Tag {
  Void   = "v",
  Unit   = "u",
  Bool   = "b",
  Int    = "i",
  Float  = "f",
  Char   = "c",
  String = "s",
  RawStr = "r"
}

export type Constant = { tag :Tag, value :string }

export const constUnit = { tag :Tag.Unit, value: "" }
export const constTrue = { tag :Tag.Bool, value: "true" }
export const constFalse = { tag :Tag.Bool, value: "false" }

export function constInt (value :number) {
  return {tag: Tag.Int, value: `${value}`}
}

export function constFloat (value :number) {
  return {tag: Tag.Int, value: `${value}`}
}

const intRe = /^-?[0-9]+$/
// TODO: this is undoubtedly WAY more complicated, whee!
const floatRe = /^-?[0-9]+\.[0-9]*?$/

export function parseLit (value :string) :Constant|void {
  // TODO: what will literal for unit be?
  if (value === "true") return constTrue
  if (value === "false") return constFalse
  if (intRe.test(value)) return {tag: Tag.Int, value}
  if (floatRe.test(value)) return {tag: Tag.Float, value}
  // TODO: what will literal for char be?
  // TODO: we want special string editing in editor
  return undefined
}
