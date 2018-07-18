import { Name } from "./names"
import { Tag, Constant } from "./constants"

// -----
// Kinds
// -----

export interface Symbol {
  readonly name :Name
}

export abstract class Kind {
}

export class Star extends Kind {
  toString () { return "*" }
}
export const star = new Star()

export class KArrow extends Kind {
  constructor (readonly from :Kind, readonly to :Kind) { super() }
  toString () { return `${this.from} → ${this.to}` }
}

export class KError extends Kind {
  constructor (readonly msg :string) { super() }
  toString () { return `!!${this.msg}!!` }
}

// -----
// Types
// -----

export abstract class Type {
  abstract get kind () :Kind
  abstract subsumes (that :Type) :boolean
  skolemize (vars :Map<Symbol, Skolem>) :Type { return this }
  map (vars :Map<Skolem, Type>) :Type { return this }
  unify (that :Type, csts :Map<Skolem, Type>) :void {
    // TODO: obvs throwing error here is not a viable error reporting strategy
    if (this.constructor != that.constructor) throw new Error(`Cannot unify ${this} with ${that}`)
  }
}

export class Hole extends Type {
  constructor (readonly kind :Kind = star) { super() }
  subsumes (that :Type) :boolean { return false }
  // TODO: correct?
  // subsumes (that :Type) :boolean { return true }
  toString () { return `Hole` }
}
export const hole = new Hole()

export class Error extends Type {
  constructor (readonly msg :string, readonly kind :Kind = star) { super() }
  subsumes (that :Type) :boolean { return false }
  toString () { return `!!${this.msg}!!` }
}

export class Const extends Type {
  get kind () { return star }
  constructor (readonly cnst :Constant) { super() }
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
  subsumes (that :Type) :boolean { return this === that }
  map (vars :Map<Skolem, Type>) :Type {
    const repl = vars.get(this)
    return repl ? repl : this
  }
  unify (that :Type, csts :Map<Skolem, Type>) :void {
    const exists = csts.get(this)
    if (exists) {
      // TODO: join that and exists
      console.log(`TODO: join ${that} and ${exists}`)
    } else {
      csts.set(this, that)
    }
  }
  toString () { return `*${this.name}` }
}

export class Arrow extends Type {
  get kind () { return star }
  constructor (readonly from :Type, readonly to :Type) { super() }
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
  subsumes (that :Type) :boolean {
    // TODO: widenings, coercions, whatnot
    return that instanceof Scalar && this.tag == that.tag && this.size == that.size
  }
  toString () { return `Scalar:${this.tag}${this.size}`}
}

export class Def extends Type {
  constructor (readonly sym :Symbol, readonly kind :Kind) { super() }
  subsumes (that :Type) :boolean {
    return that instanceof Def && this.sym == that.sym
  }
  toString () { return `${this.sym.name}`}
}

// TODO: prod and sym are not really types, should they maybe go away and we obtain this
// information via the symbol defined by the appropriate type tree?
export class Prod extends Type {
  get kind () { return star }
  constructor (readonly fields :Def[]) { super() }
  subsumes (that :Type) :boolean { return false }
  toString () { return `Record${this.fields.length}` }
}

export class Sum extends Type {
  get kind () { return star }
  constructor (readonly cases :Def[]) { super() }
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
