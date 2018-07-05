import { Name } from './names'
import { Type, hole } from './types'

export type Kind = "term" | "type"

export interface Symbol {
  readonly kind :Kind
  readonly name :Name
  readonly type :Type
}

export const noTermSym :Symbol = {kind: "term", name: "<noterm>", type: hole}
export const noTypeSym :Symbol = {kind: "type", name: "<notype>", type: hole}

export function missingSym (kind :Kind, name :Name) :Symbol {
  return {kind, name: `<missing: ${name}>`, type: hole}
}

export class ModSym implements Symbol {
  constructor (readonly name :Name) {}
  get kind () :Kind { return "term" }
  get type () :Type { return hole } // TODO: special module type? none type?
  toString () { return this.name }
}

export interface TermSym extends Symbol {
  readonly kind :"term"
  readonly owner :TermSym|ModSym
}

export interface TypeSym extends Symbol {
  readonly kind :"type"
  readonly owner :TypeSym|ModSym
}

export class Scope {

  constructor (readonly parent :Scope|void, readonly symbols :Symbol[]) {}

  lookupTerm (name :Name) :TermSym { return this.lookup("term", name) as TermSym }
  lookupType (name :Name) :TypeSym { return this.lookup("type", name) as TypeSym }

  lookup (kind :Kind, name :Name) :Symbol {
    const sym = this.symbols.find(sym => sym.name === name)
    const parent = this.parent
    if (sym && sym.kind === kind) return sym
    else if (parent) return parent.lookup(kind, name)
    else return missingSym(kind, name)
  }

  extend (symbols :Symbol[]) :Scope {
    return new Scope(this, symbols)
  }
}

export const emptyScope = new Scope(undefined, [])
