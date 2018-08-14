import { observable } from 'mobx'
import { Name } from './names'
import { Type, Const, hole } from './types'
import { Constant } from './constants'

export type Kind = "term" | "type" | "module"
export type Flavor = "none" | "cnst" | "func" | "ctor"

/** Defines the API needed for symbols to register themselves with their module's index (when
  * created), and remove themselves (when destroyed). */
export interface Index {
  /** Returns a peristent id for use by a new definition symbol in a module.
    * The symbol created with this id must be `insert`ed before the next call to `nextSymId`. */
  nextSymId () :number
  /** Inserts `sym` into this index. */
  insert (sym :Symbol) :void
  /** Removes `sym` from this index. */
  remove (sym :Symbol) :void
}

export abstract class Symbol {
  /** The human readable name for this symbol. */
  @observable name :Name

  constructor (
    /** Whether this symbol represents a term, type or module. */
    readonly kind :Kind,
    /** Ad-hoc further refinement of this symbol's kind. */
    readonly flavor :Flavor,
    /** This symbol's persistent id. Only non-`0` for symbols that refer to trees. */
    readonly id :number,
    name :Name) {
    this.name = name
  }

  get displayName () :string { return this.name }

  abstract get type () :Type
  abstract get owner () :Symbol

  /** Returns the root of this symbol's ownership chain. For most symbols this will be a module
    * symbol, but some symbols can have esoteric owners (like missing syms, holes, and consts). */
  get root () :Symbol {
    let sym :Symbol = this, owner = this.owner
    while (sym !== owner) {
      sym = owner
      owner = sym.owner
    }
    return sym
  }

  /** Whether or not this symbol is lexically scoped. Lexically scoped symbols will only ever be
    * referenced in their lexical scope. Non-lexically scoped symbols may be referenced anywhere in
    * their module (or in other modules if they are exported). */
  get lexical () :boolean { return this.owner.kind !== "module" }

  get index () :Index { return this.owner.index }

  get isHole () :boolean { return this.name === "" }

  toString () {
    return `${this.name}#${this.lexical ? "l" : "m"}${this.id}`
  }
}

export class MissingSym extends Symbol {
  constructor (kind :Kind, name :Name) { super(kind, "none", 0, name) }
  get displayName () :string { return `<missing: ${name}>` }
  get type () { return hole }
  get owner () :Symbol { return this }
  get index () :Index { throw new Error(`Cannot index through ${this}`) }
}

export class TermHoleSym extends Symbol {
  constructor () { super("term", "none", 0, "") }
  get displayName () :string { return "?" }
  get type () { return hole }
  get owner () { return this } // TODO: proper owner?
  get index () :Index { throw new Error(`Cannot index through ${this}`) }
}

export class TypeHoleSym extends Symbol {
  constructor () { super("type", "none", 0, "") }
  get displayName () :string { return "?" }
  get type () { return hole }
  get owner () { return this } // TODO: proper owner?
  get index () :Index { throw new Error(`Cannot index through ${this}`) }
}

export class TermConstSym extends Symbol {
  constructor (readonly cnst :Constant) { super("term", "cnst", 0, cnst.value) }
  get type () { return new Const(this.cnst) }
  get owner () { return this } // TODO: none?
  get index () :Index { throw new Error(`Cannot index through ${this}`) }
}

export class TypeConstSym extends Symbol {
  constructor (readonly cnst :Constant) { super("type", "cnst", 0, cnst.value) }
  get type () { return new Const(this.cnst) }
  get owner () { return this } // TODO: none?
  get index () :Index { throw new Error(`Cannot index through ${this}`) }
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

class EmptyScope extends Scope {
  lookup (kind :Kind, name :Name) :Symbol { return new MissingSym(kind, name) }
  toString () { return `<empty>` }
  _addCompletions (pred :(sym :Symbol) => Boolean, prefix :string, syms :Symbol[]) {}
}
export const emptyScope :Scope = new EmptyScope()

class EmptySym extends Symbol {
  get owner () :Symbol { return this }
  get type () :Type { return hole }
  get index () :Index { throw new Error(`Cannot index through ${this}`) }
}
export const emptySym :Symbol = new EmptySym("term", "none", 0, "<empty>")
