import { observable, observe } from 'mobx'
import { Name } from './names'
import { Type, Const, hole } from './types'
import { Constant } from './constants'

export type Kind = "term" | "type" | "module"
export type Flavor = "none" | "func" | "ctor" | "cnst"

let symId = 0
function nextSymId () { symId += 1 ; return symId }

export abstract class Symbol {
  readonly id :number = nextSymId()
  @observable name :Name

  constructor (readonly kind :Kind, readonly flavor :Flavor, name :Name) {
    this.name = name
  }

  get displayName () :string { return this.name }
  abstract get type () :Type
  abstract get owner () :Symbol

  get isHole () :boolean { return this.name === "" }
  get module () :ModuleSym {
    let osym = this.owner
    while (osym.kind !== "module") {
      osym = osym.owner
    }
    return osym as ModuleSym
  }

  toString () { return `${this.name}#${this.id}` }
}

export class MissingSym extends Symbol {
  constructor (kind :Kind, name :Name) { super(kind, "none", name) }
  get displayName () :string { return `<missing: ${name}>` }
  get type () { return hole }
  get owner () :Symbol { return this}
}

export class TermHoleSym extends Symbol {
  constructor () { super("term", "none", "") }
  get displayName () :string { return "?" }
  get type () { return hole }
  get owner () { return this } // TODO: proper owner?
}

export class TypeHoleSym extends Symbol {
  constructor () { super("type", "none", "") }
  get displayName () :string { return "?" }
  get type () { return hole }
  get owner () { return this } // TODO: proper owner?
}

export class TermConstSym extends Symbol {
  constructor (readonly cnst :Constant) { super("term", "cnst", cnst.value) }
  get type () { return new Const(this.cnst) }
  get owner () { return this } // TODO: none?
}

export class TypeConstSym extends Symbol {
  constructor (readonly cnst :Constant) { super("type", "cnst", cnst.value) }
  get type () { return new Const(this.cnst) }
  get owner () { return this } // TODO: none?
}

export class ModuleSym extends Symbol {
  readonly scope = new ModuleScope(this)
  constructor (name :Name) { super("module", "none", name) }
  get type () :Type { return hole } // TODO: special module type? none type?
  get owner () :Symbol { return this }
  toString () { return `msym#${this.id}:${this.name}` }
}

export abstract class Scope {

  lookupTerm (name :Name) :Symbol { return this.lookup("term", name) }
  lookupType (name :Name) :Symbol { return this.lookup("type", name) }

  abstract lookup (kind :Kind, name :Name) :Symbol

  getCompletions (pred :(sym :Symbol) => Boolean, prefix :string) {
    const syms :Symbol[] = []
    this._addCompletions(pred, prefix.toLowerCase(), syms)
    return syms
  }

  toString () { return this.constructor.name }

  abstract _addCompletions (pred :(sym :Symbol) => Boolean, prefix :string, syms :Symbol[]) :void

  protected isCompletion (prefix :string, sym :Symbol) :boolean {
    return !sym.isHole && sym.name.toLowerCase().startsWith(prefix)
  }
}

type Disposer = () => void

// NOTE: this will eventually be subsumed by a persistent project-wide symbol database
export class ModuleScope extends Scope {
  // TODO: we really want a tree map or something we can prefix query
  symbols = new Map<Name,Symbol[]>()
  listeners = new Map<Symbol,Disposer>()

  constructor (readonly owner :ModuleSym) { super() }

  // TODO: revamp to return all matching symbols
  lookup (kind :Kind, name :Name) :Symbol {
    const syms = this.symbols.get(name)
    if (syms) {
      for (let sym of syms) {
        if (sym.kind == kind) return sym
      }
    }
    return new MissingSym(kind, name)
  }

  insert (sym :Symbol) {
    if (sym.name !== "") this.map(sym.name, sym)
    this.listeners.set(sym, observe(sym, "name", change => {
      if (change.oldValue && change.oldValue !== "") this.unmap(change.oldValue, sym)
      if (change.newValue !== "") this.map(change.newValue, sym)
    }))
  }

  remove (sym :Symbol) {
    if (sym.name !== "") this.unmap(sym.name, sym)
    const disp = this.listeners.get(sym)
    disp && disp()
  }

  toString () :string {
    return `<module:${this.owner.name}>`
  }

  _addCompletions (pred :(sym :Symbol) => Boolean, prefix :string, syms :Symbol[]) {
    // TODO: such inefficient, so expense
    for (let symvec of Array.from(this.symbols.values())) {
      for (let sym of symvec) {
        if (pred(sym) && this.isCompletion(prefix, sym)) {
          syms.push(sym)
        }
      }
    }
  }

  private map (name :Name, sym :Symbol) {
    const syms = this.symbols.get(name)
    if (syms) syms.push(sym)
    else this.symbols.set(name, [sym])
  }

  private unmap (name :Name, sym :Symbol) {
    const syms = this.symbols.get(name)
    if (syms) {
      const idx = syms.indexOf(sym)
      if (idx >= 0) syms.splice(idx, 1)
    }
  }
}

class EmptyScope extends Scope {
  lookup (kind :Kind, name :Name) :Symbol { return new MissingSym(kind, name) }
  toString () { return `<empty>` }
  _addCompletions (pred :(sym :Symbol) => Boolean, prefix :string, syms :Symbol[]) {}
}
export const emptyScope :Scope = new EmptyScope()
