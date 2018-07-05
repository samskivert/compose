
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
