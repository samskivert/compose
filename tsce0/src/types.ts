import { Name } from "./names"
import { Tag, Constant } from "./constants"

// -----
// Kinds
// -----

export interface Symbol {
  readonly name :Name
}

export interface DefTree {
  readonly body :{sig :Type}
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
  constructor (readonly arg :Kind, readonly res :Kind) { super() }
  equals (that :Kind) :boolean {
    return that instanceof KArrow && this.arg.equals(that.arg) && this.res.equals(that.res)
  }
  toString () { return `${this.arg} → ${this.res}` }
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
  get isError () :boolean { return false }
  get isArrow () :boolean { return false }
  get arity () :number { return 0 }
  abstract subsumes (that :Type) :boolean
  abstract equals (that :Type) :boolean
  join (that :Type) :Type {
    if (this.equals(that)) return this
    else if (that instanceof Hole) return this
    else if (that instanceof Def) return that.join(this)
    else return this._joinFailure(that)
  }
  protected _joinFailure (that :Type) :Type {
    return new Error(`Cannot join '${this}' with '${that}'`)
  }
  skolemize (vars :Map<Symbol, Skolem>) :Type { return this }
  map (vars :Map<Skolem, Type>) :Type { return this }
  unify (that :Type, csts :Map<Skolem, Type>, errors :string[]) :void {
    // TODO: obvs throwing error here is not a viable error reporting strategy
    if (this.constructor != that.constructor) errors.push(`Cannot unify ${this} with ${that}`)
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
  get isError () :boolean { return true }
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
  unify (that :Type, csts :Map<Skolem, Type>, errors :string[]) :void {
    const exists = csts.get(this)
    csts.set(this, exists ? that.join(exists) : that)
  }
  toString () { return `*${this.name}` }
}

export class Arrow extends Type {
  constructor (readonly arg :Type, readonly res :Type) { super() }
  get kind () { return star }
  get isArrow () :boolean { return true }
  get arity () :number { return 1 + this.res.arity }
  get finalResult () :Type {
    return (this.res instanceof Arrow) ? this.res.finalResult : this.res
  }
  equals (that :Type) :boolean {
    return that instanceof Arrow && this.arg.equals(that.arg) && this.res.equals(that.res)
  }
  join (that :Type) :Type {
    return (that instanceof Arrow) ? new Arrow(this.arg.join(that.arg), this.res.join(that.res)) :
      super.join(that)
  }
  subsumes (that :Type) :boolean {
    if (that instanceof Arrow) {
      return this.arg.subsumes(that.arg) && that.res.subsumes(this.res)
    }
    return false
  }
  skolemize (vars :Map<Symbol, Skolem>) :Type {
    return new Arrow(this.arg.skolemize(vars), this.res.skolemize(vars))
  }
  map (vars :Map<Skolem, Type>) :Type {
    return new Arrow(this.arg.map(vars), this.res.map(vars))
  }
  unify (that :Type, csts :Map<Skolem, Type>, errors :string[]) :void {
    if (that instanceof Arrow) {
      this.arg.unify(that.arg, csts, errors)
      this.res.unify(that.res, csts, errors)
    } else super.unify(that, csts, errors)
  }
  toString () { return `${this.arg} → ${this.res}`}
}

export class App extends Type {
  get kind () { return kindApply(this.ctor.kind, this.arg.kind) }
  constructor (readonly ctor :Type, readonly arg :Type) { super () }
  equals (that :Type) :boolean {
    return that instanceof App && this.ctor.equals(that.ctor) && this.arg.equals(that.arg)
  }
  join (that :Type) :Type {
    return (that instanceof App) ? new App(this.ctor.join(that.ctor), this.arg.join(that.arg)) :
      super.join(that)
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
  unify (that :Type, csts :Map<Skolem, Type>, errors :string[]) :void {
    if (that instanceof App) {
      this.ctor.unify(that.ctor, csts, errors)
      this.arg.unify(that.arg, csts, errors)
    } else super.unify(that, csts, errors)
  }
  toString () { return `${this.ctor} ${this.arg}`}
}

export class Abs extends Type {
  constructor (readonly sym :Symbol, readonly body :Type) { super() }
  get kind () { return new KArrow(star, this.body.kind) }
  get isArrow () :boolean { return this.body.isArrow }
  get arity () :number { return this.body.arity }
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
  get kind () :Kind { return this.tree.body.sig.kind }
  constructor (readonly sym :Symbol, readonly tree :DefTree) { super() }
  equals (that :Type) :boolean { return that instanceof Def && this.sym === that.sym }
  join (that :Type) :Type {
    if (this.subsumes(that)) return this
    else if (that instanceof Hole) return this
    else return this._joinFailure(that)
  }
  subsumes (that :Type) :boolean {
    return (that instanceof Def && this.sym == that.sym) || this.tree.body.sig.subsumes(that)
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
  else return fun.res
}

export function unifyApply (uleft :Type, uright :Type, aleft :Type) :Type {
  const mappings = new Map()
  const errors :string[] = []
  uleft.unify(uright, mappings, errors)
  // TODO: should we report all unification errors?
  if (errors.length > 0) return new Error(errors[0])
  return aleft.map(mappings)
}

export function funApply (fun :Type, arg :Type) :Type {
  // if the function is generalized...
  if (fun instanceof Abs) {
    // we have to skolemize...
    const skfun = fun.skolemize(new Map())
    if (!(skfun instanceof Arrow)) return new Error(
      `Cannot apply arg (type: ${arg}) to non-fun: ${skfun}`)
    // unify...
    const ufun = unifyApply(skfun.arg, arg, skfun)
    if (!(ufun instanceof Arrow)) return ufun // error
    // and regeneralize... (TODO)
    return arrowApply(ufun, arg)
  }
  else if (!(fun instanceof Arrow)) return new Error(
    `Cannot apply arg (type: ${arg}) to non-fun: ${fun}`)
  else return arrowApply(fun, arg)
}

export function arrowApply (fun :Arrow, arg :Type) :Type {
  // if arg is generalized...
  if (arg instanceof Abs) {
    const skarg = arg.skolemize(new Map())
    const uarg = unifyApply(skarg, fun.res, skarg)
    // TODO: regeneralize?
    return arrowApply(fun, uarg)
  }
  else if (!fun.arg.subsumes(arg)) return new Error(
    `Cannot apply fun of type ${fun} to arg of type ${arg}`)
  else return fun.res
}

export function patUnapply (type :Type) :Type {
  if (!(type instanceof Arrow)) return new Error(`Cannot unapply non-arrow type ${type}`)
  else if (type.res instanceof Arrow) return new Arrow(type.arg, patUnapply(type.res))
  else return type.arg
}

export function patLastArg (type :Type) :Type {
  if (!(type instanceof Arrow)) return new Error(`Cannot get last arg of non-arrow type ${type}`)
  else if (type.res instanceof Arrow) return patLastArg(type.res)
  else return type.res
}

export function patFlipArrow (type :Type) :Type {
  function flip (from :Type, to :Type) :Type {
    if (from instanceof Arrow) return flip(from.res, new Arrow(from.arg, to))
    else return new Arrow(from, to)
  }
  if (type instanceof Arrow) return flip(type.res, type.arg)
  else return type
}

export function patUnify (ctorType :Type, prototype :Type) :Type {
  if (ctorType instanceof Abs) {
    const skCtor = patFlipArrow(ctorType.skolemize(new Map()))
    // console.log(`patUnify ${ctorType} => ${patFlipArrow(skCtor)}`)
    const uCtor = unifyApply(skCtor instanceof Arrow ? skCtor.arg : skCtor, prototype, skCtor)
    // TODO: regeneralize?
    return uCtor
  } else {
    return ctorType
  }
}

export function patApplies (ctorType :Type, scrutType :Type) :boolean {
  const uCtorType = patUnify(ctorType, scrutType)
  // TODO: is equals the right check here? join?
  if (uCtorType instanceof Arrow) {
    return uCtorType.arg.equals(scrutType)
  } else {
    return uCtorType.equals(scrutType)
  }
}
