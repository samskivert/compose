import { Name } from "./names"
import { Tag, Constant } from "./constants"

// -----
// Kinds
// -----

export interface Symbol {
  readonly name :Name
}

export interface DefSymbol extends Symbol {
  readonly bodyType :Type
}

export abstract class Kind {
  abstract equals (that :Kind) :boolean
}

export class Star extends Kind {
  equals (that :Kind) :boolean { return that instanceof Star }
  toString () { return "*" }
}
export const star = new Star()

export class KArrow extends Kind {
  constructor (readonly from :Kind, readonly to :Kind) { super() }
  equals (that :Kind) :boolean {
    return that instanceof KArrow && this.from.equals(that.from) && this.to.equals(that.to)
  }
  toString () { return `${this.from} → ${this.to}` }
}

export class KError extends Kind {
  constructor (readonly msg :string) { super() }
  equals (that :Kind) :boolean { return false }
  toString () { return `!!${this.msg}!!` }
}

// -----
// Types
// -----

export abstract class Type {
  abstract get kind () :Kind
  abstract subsumes (that :Type) :boolean
  abstract equals (that :Type) :boolean
  join (that :Type) :Type {
    if (this.equals(that)) return this
    else if (that instanceof Def) return that.join(this)
    else return this._joinFailure(that)
  }
  protected _joinFailure (that :Type) :Type {
    return new Error(`Cannot join '${this}' with '${that}'`)
  }
  skolemize (vars :Map<Symbol, Skolem>) :Type { return this }
  map (vars :Map<Skolem, Type>) :Type { return this }
  unify (that :Type, csts :Map<Skolem, Type>) :void {
    // TODO: obvs throwing error here is not a viable error reporting strategy
    if (this.constructor != that.constructor) throw new Error(`Cannot unify ${this} with ${that}`)
  }
}

export class Hole extends Type {
  constructor (readonly kind :Kind = star) { super() }
  equals (that :Type) :boolean {
    return that instanceof Hole && this.kind.equals(that.kind)
  }
  subsumes (that :Type) :boolean { return false }
  join (that :Type) :Type { return that }
  // TODO: correct?
  // subsumes (that :Type) :boolean { return true }
  toString () { return `Hole` }
}
export const hole = new Hole()

export class Error extends Type {
  constructor (readonly msg :string, readonly kind :Kind = star) { super() }
  equals (that :Type) :boolean { return false }
  join (that :Type) :Type { return this }
  subsumes (that :Type) :boolean { return false }
  toString () { return `!!${this.msg}!!` }
}

export class Const extends Type {
  get kind () { return star }
  constructor (readonly cnst :Constant) { super() }
  equals (that :Type) :boolean { return that instanceof Const && this.cnst.equals(that.cnst) }
  join (that :Type) :Type {
    if (that instanceof Const) {
      if (this.cnst.tag != that.cnst.tag) return super.join(that)
      else if (this.cnst.value === that.cnst.value) return this
      else return new Scalar(this.cnst.tag, Math.max(this.cnst.bitWidth, that.cnst.bitWidth))
    }
    else return that.join(this)
  }
  subsumes (that :Type) :boolean {
    if (that instanceof Const) {
      return (this.cnst.tag === that.cnst.tag &&
              this.cnst.value === that.cnst.value)
    }
    return false
  }
  toString () { return `'${this.cnst.value}`}
}

export class Var extends Type {
  get kind () { return star }
  constructor (readonly sym :Symbol) { super() }
  equals (that :Type) :boolean { return that instanceof Var && this.sym === that.sym }
  skolemize (vars :Map<Symbol, Skolem>) :Type {
    const skolem = vars.get(this.sym)
    return skolem ? skolem : this
  }
  subsumes (that :Type) :boolean {
    return (that instanceof Var) && this.sym == that.sym
  }
  toString () { return `${this.sym.name}` }
}

export class Skolem extends Type {
  get kind () { return star }
  constructor (readonly name :Name) { super() }
  equals (that :Type) :boolean { return this === that }
  subsumes (that :Type) :boolean { return this === that }
  map (vars :Map<Skolem, Type>) :Type {
    const repl = vars.get(this)
    return repl ? repl : this
  }
  unify (that :Type, csts :Map<Skolem, Type>) :void {
    const exists = csts.get(this)
    csts.set(this, exists ? that.join(exists) : that)
  }
  toString () { return `*${this.name}` }
}

export class Arrow extends Type {
  get kind () { return star }
  constructor (readonly from :Type, readonly to :Type) { super() }
  get finalResult () :Type {
    return (this.to instanceof Arrow) ? this.to.finalResult : this.to
  }
  equals (that :Type) :boolean {
    return that instanceof Arrow && this.from.equals(that.from) && this.to.equals(that.to)
  }
  join (that :Type) :Type {
    if (that instanceof Arrow) return new Arrow(this.from.join(that.from), this.to.join(that.to))
    else return super.join(that)
  }
  subsumes (that :Type) :boolean {
    if (that instanceof Arrow) {
      return this.from.subsumes(that.from) && that.to.subsumes(this.to)
    }
    return false
  }
  skolemize (vars :Map<Symbol, Skolem>) :Type {
    return new Arrow(this.from.skolemize(vars), this.to.skolemize(vars))
  }
  map (vars :Map<Skolem, Type>) :Type {
    return new Arrow(this.from.map(vars), this.to.map(vars))
  }
  unify (that :Type, csts :Map<Skolem, Type>) :void {
    if (that instanceof Arrow) {
      this.from.unify(that.from, csts)
      this.to.unify(that.to, csts)
    } else super.unify(that, csts)
  }
  toString () { return `${this.from} → ${this.to}`}
}

export class App extends Type {
  get kind () { return kindApply(this.ctor.kind, this.arg.kind) }
  constructor (readonly ctor :Type, readonly arg :Type) { super () }
  equals (that :Type) :boolean {
    return that instanceof App && this.ctor.equals(that.ctor) && this.arg.equals(that.arg)
  }
  join (that :Type) :Type {
    if (that instanceof App) return new App(this.ctor.join(that.ctor), this.arg.join(that.arg))
    else return super.join(that)
  }
  subsumes (that :Type) :boolean {
    if (that instanceof App) return this.ctor.subsumes(that.ctor) && this.arg.subsumes(that.arg)
    else return false
  }
  skolemize (vars :Map<Symbol, Skolem>) :Type {
    return new App(this.ctor.skolemize(vars), this.arg.skolemize(vars))
  }
  map (vars :Map<Skolem, Type>) :Type {
    return new App(this.ctor.map(vars), this.arg.map(vars))
  }
  unify (that :Type, csts :Map<Skolem, Type>) :void {
    if (that instanceof App) {
      this.ctor.unify(that.ctor, csts)
      this.arg.unify(that.arg, csts)
    } else super.unify(that, csts)
  }
  toString () { return `${this.ctor} ${this.arg}`}
}

export class Abs extends Type {
  get kind () { return new KArrow(star, this.body.kind) }
  constructor (readonly sym :Symbol, readonly body :Type) { super() }
  equals (that :Type) :boolean {
    // TODO: can the symbols be equal but the body not equal?
    return that instanceof Abs && this.sym === that.sym && this.body.equals(that.body)
  }
  // TODO: can we join type abstractions?
  // TODO: does abs subsume anything
  subsumes (that :Type) :boolean { return false }
  skolemize (vars :Map<Symbol, Skolem>) :Type {
    const skolem = new Skolem(this.sym.name)
    vars.set(this.sym, skolem)
    return this.body.skolemize(vars)
  }
  // TODO: how to unify under an abs? just ignore the abs?
  toString () { return `∀${this.sym.name} ${this.body}`}
}

export class Scalar extends Type {
  get kind () { return star }
  constructor (readonly tag :Tag, readonly size :number) { super () }
  equals (that :Type) :boolean {
    return that instanceof Scalar && this.tag === that.tag && this.size == that.size
  }
  join (that :Type) :Type {
    if (that instanceof Scalar) {
      if (this.tag !== that.tag) return super.join(that)
      else return this.size > that.size ? this : that
    } else if (that instanceof Const) {
      if (this.tag !== that.cnst.tag) return super.join(that)
      // TODO: will tag dictate signed versus unsigned?
      else return this
    }
    else return super.join(that)
  }
  subsumes (that :Type) :boolean {
    if (that instanceof Const) {
      if (this.tag !== that.cnst.tag) return false
      // TODO: will tag dictate signed versus unsigned?
      else return true
    }
    // TODO: widenings, coercions, whatnot
    return that instanceof Scalar && this.tag == that.tag && this.size >= that.size
  }
  toString () { return `Scalar:${this.tag}${this.size}`}
}

export class Def extends Type {
  get kind () :Kind { return this.sym.bodyType.kind }
  constructor (readonly sym :DefSymbol) { super() }
  equals (that :Type) :boolean { return that instanceof Def && this.sym === that.sym }
  join (that :Type) :Type {
    if (this.subsumes(that)) return this
    else return this._joinFailure(that)
  }
  subsumes (that :Type) :boolean {
    return (that instanceof Def && this.sym == that.sym) || this.sym.bodyType.subsumes(that)
  }
  toString () { return `${this.sym.name}`}
}

// TODO: prod and sym are not really types, should they maybe go away and we obtain this
// information via the symbol defined by the appropriate type tree?
export class Prod extends Type {
  get kind () { return star }
  constructor (readonly fields :Def[]) { super() }
  equals (that :Type) :boolean {
    return that instanceof Prod && this.fields.every((f, ii) => f.equals(that.fields[ii]))
  }
  subsumes (that :Type) :boolean { return false }
  toString () { return `Record${this.fields.length}` }
}

export class Sum extends Type {
  get kind () { return star }
  constructor (readonly cases :Def[]) { super() }
  equals (that :Type) :boolean {
    return that instanceof Sum && this.cases.every((c, ii) => c.equals(that.cases[ii]))
  }
  subsumes (that :Type) :boolean { return false }
  toString () { return `Sum${this.cases.length}` }
}

// -- | Array Type?
// -- | Interface Name Params Methods
// -- | Method Name Type

export function kindApply (fun :Kind, arg :Kind) :Kind {
  if (!(fun instanceof KArrow)) return new Error(
    `Cannot apply type arg (kind: ${arg}) to non-arrow kind ${fun}`)
  else if (arg !== star) return new Error(
    `Cannot apply type arrow (${fun}) to non-star kind ${arg}`)
  else return fun.to
}

export function funApply (fun :Type, arg :Type) :Type {
  // if the function is generalized...
  if (fun instanceof Abs) {
    // we have to skolemize...
    const skfun = fun.skolemize(new Map())
    if (!(skfun instanceof Arrow)) return new Error(
      `Cannot apply arg (type: ${arg}) to non-fun: ${skfun}`)
    // unfiy...
    const mappings = new Map()
    skfun.from.unify(arg, mappings)
    const ufun = skfun.map(mappings)
    // and regeneralize... (TODO)
    return arrowApply(ufun as Arrow, arg)
  }
  else if (!(fun instanceof Arrow)) return new Error(
    `Cannot apply arg (type: ${arg}) to non-fun: ${fun}`)
  else return arrowApply(fun, arg)
}

export function arrowApply (fun :Arrow, arg :Type) :Type {
  if (!fun.from.subsumes(arg)) return new Error(
    `Cannot apply fun of type ${fun} to arg of type ${arg}`)
  else return fun.to
}

export function patUnapply (type :Type) :Type {
  if (!(type instanceof Arrow)) return new Error(`Cannot unapply non-arrow type ${type}`)
  else if (type.to instanceof Arrow) return new Arrow(type.from, patUnapply(type.to))
  else return type.from
}

export function patLastArg (type :Type) :Type {
  if (!(type instanceof Arrow)) return new Error(`Cannot get last arg of non-arrow type ${type}`)
  else if (type.to instanceof Arrow) return patLastArg(type.to)
  else return type.to
}

export function patFlipArrow (type :Type) :Type {
  function flip (from :Type, to :Type) :Type {
    if (from instanceof Arrow) return flip(from.to, new Arrow(from.from, to))
    else return new Arrow(from, to)
  }
  if (type instanceof Arrow) return flip(type.to, type.from)
  else return type
}

export function patUnify (ctorType :Type, prototype :Type) :Type {
  if (ctorType instanceof Abs) {
    const skCtor = patFlipArrow(ctorType.skolemize(new Map()))
    console.log(`patUnify ${ctorType} => ${patFlipArrow(skCtor)}`)
    const mappings = new Map()
    if (skCtor instanceof Arrow) skCtor.from.unify(prototype, mappings)
    else skCtor.unify(prototype, mappings)
    const uCtor = skCtor.map(mappings)
    // TODO: regeneralize?
    return uCtor
  } else {
    return ctorType
  }
}
