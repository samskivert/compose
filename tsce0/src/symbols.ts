import { observable, observe } from 'mobx'
import { Name } from './names'
import { Type, hole } from './types'

export type Kind = "term" | "func" | "type" | "module"

let symId = 0
function nextSymId () { symId += 1 ; return symId }

export abstract class Symbol {
  readonly id :number = nextSymId()
  @observable name :Name

  constructor (readonly kind :Kind, name :Name) {
    this.name = name
  }

  abstract get type () :Type
  abstract get owner () :Symbol

  get module () :ModuleSym {
    let osym = this.owner
    while (osym.kind !== "module") {
      osym = osym.owner
    }
    return osym as ModuleSym
  }

  toString () { return `sym#${this.id}:${this.name}` }
}

export class MissingSym extends Symbol {
  constructor (kind :Kind, name :Name) {
    super(kind, `<missing: ${name}>`)
  }
  get type () { return hole }
  get owner () :Symbol { return this}
}

export class HoleSym extends Symbol {
  constructor () {
    super ("term", "")
  }
  get type () { return hole }
  get owner () { return this }
}

export class ModuleSym extends Symbol {
  readonly scope = new ModuleScope(this)
  constructor (name :Name) { super("module", name) }
  get type () :Type { return hole } // TODO: special module type? none type?
  get owner () :Symbol { return this }
  toString () { return `msym#${this.id}:${this.name}` }
}

export abstract class Scope {

  lookupTerm (name :Name) :Symbol { return this.lookup("term", name) }
  lookupFunc (name :Name) :Symbol { return this.lookup("func", name) }
  lookupType (name :Name) :Symbol { return this.lookup("type", name) }

  abstract lookup (kind :Kind, name :Name) :Symbol

  getCompletions (pred :(sym :Symbol) => Boolean, prefix :string) {
    const syms :Symbol[] = []
    this._addCompletions(pred, prefix.toLowerCase(), syms)
    return syms
  }

  abstract _addCompletions (pred :(sym :Symbol) => Boolean, prefix :string, syms :Symbol[]) :void

  protected isCompletion (prefix :string, name :Name) :boolean {
    return name.toLowerCase().startsWith(prefix)
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
        if (pred(sym) && this.isCompletion(prefix, sym.name)) {
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
  _addCompletions (pred :(sym :Symbol) => Boolean, prefix :string, syms :Symbol[]) {}
}
export const emptyScope :Scope = new EmptyScope()
