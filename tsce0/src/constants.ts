
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

export class Constant {
  constructor (readonly tag :Tag, readonly value :string) {}

  equals (that :Constant) :boolean {
    return this.tag === that.tag && this.value === that.value
  }

  get bitWidth () :number {
    switch (this.tag) {
    case Tag.Void:   return 0
    case Tag.Unit:   return 0
    case Tag.Bool:   return 1
    case Tag.Int:    return intWidth(this.value)
    case Tag.Float:  return floatWidth(this.value)
    case Tag.Char:   return 8 // TODO: 16? do we even want a 'char' type?
    case Tag.String: return 0 // TODO
    case Tag.RawStr: return 0 // TODO
    default:         throw new Error(`Unexpected constant tag '${this.tag}'`)
    }
  }

  toString () { return `${this.tag}#${this.value}` }
}

export const constUnit = new Constant(Tag.Unit, "")
export const constTrue = new Constant(Tag.Bool, "true")
export const constFalse = new Constant(Tag.Bool, "false")

export function constInt (value :number) {
  return new Constant(Tag.Int, `${value}`)
}

export function constFloat (value :number) {
  return new Constant(Tag.Float, `${value}`)
}

const intRe = /^-?[0-9]+$/
// TODO: this is undoubtedly WAY more complicated, whee!
const floatRe = /^-?[0-9]+\.[0-9]*?$/

function intWidth (value :string) :number {
  return 32 // TODO
}
function floatWidth (value :string) :number {
  return 32 // TODO
}

export function parseLit (value :string) :Constant|void {
  // TODO: what will literal for unit be?
  if (value === "true") return constTrue
  if (value === "false") return constFalse
  if (intRe.test(value)) return new Constant(Tag.Int, value)
  if (floatRe.test(value)) return new Constant(Tag.Float, value)
  // TODO: what will literal for char be?
  // TODO: we want special string editing in editor
  return undefined
}

export function inflateConst (json :any) {
  return new Constant(json.tag as Tag, json.value as string)
}
