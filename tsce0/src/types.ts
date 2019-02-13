import { Name } from "./names"
import { Tag, Constant } from "./constants"

// -----
// Kinds
// -----

export interface Symbol {
  readonly name :Name
}

export interface Tree {
  /** Checks that this tree has type `tpe` with input context `ctx`.
    * @return the output context or an error. */
  check (ctx :Context, tpe :Type) :Context|string
}

export interface DefTree {
  readonly signature :Type
}

export function check (tree :Tree, checkCtx :Context, checkType :Type, resType :Type,
                       peeler :(ctx :Context) => Context = ctx => ctx) :[Type, Context] {
  const checkedCtx = tree.check(checkCtx, checkType)
  return typeof checkedCtx === 'string' ?
    [new Error(checkedCtx), checkCtx] :
    [resType, peeler(checkedCtx)]
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

function kindApply (fun :Kind, arg :Kind) :Kind {
  if (!(fun instanceof KArrow)) return new Error(
      `Cannot apply type arg (kind: ${arg}) to non-arrow kind ${fun}`)
  else if (arg !== star) return new Error(
    `Cannot apply type arrow (${fun}) to non-star kind ${arg}`)
  else return fun.res
}

// -----
// Types
// -----

interface Note {
  equals (other :Note) :boolean
}

export class NAssump implements Note {
  constructor (readonly sym :Symbol, readonly tpe :Type) {}
  equals (other :Note) :boolean { return (other instanceof NAssump &&
                                          other.sym === this.sym &&
                                          other.tpe.equals(this.tpe)) }
  toString () { return `${this.sym}~${this.tpe}` }
}
class NSol implements Note {
  constructor (readonly ev :EVar, readonly tpe :Type) {}
  equals (other :Note) :boolean { return (other instanceof NSol &&
                                          other.ev.equals(this.ev) &&
                                          other.tpe.equals(this.tpe)) }
  toString () { return `${this.ev}=${this.tpe}` }
}
class NMark implements Note {
  constructor (readonly ev :EVar) {}
  equals (other :Note) :boolean { return (other instanceof NMark &&
                                          other.ev === this.ev) }
  toString () { return `*${this.ev}` }
}
// UVar and EVar are also notes

function collect<A, B> (as :A[], pred :(a :A) => B|undefined) :B[] {
  let matches :B[] = []
  for (let a of as) {
    const res = pred(a)
    res && matches.push(res)
  }
  return matches
}

export class Tracer {
  msgs :string[] = []
  trace (msg :string) {
    this.msgs.push(msg)
  }
}

var nextEVar = 1
export function freshEVar (name :string) :EVar {
  try { return new EVar(`${name}${nextEVar}`) }
  finally { nextEVar += 1 }
}

export class Context {
  constructor (readonly tracer :Tracer, readonly notes :Note[] = []) {}

  private indexOf (note :Note) :number {
    for (let ii = 0, ll = this.notes.length; ii < ll; ii += 1) {
      let n = this.notes[ii]
      if (n.equals(note)) return ii
    }
    return -1
  }

  /** Returns whether this context contains `note`. */
  contains (note :Note) :boolean { return this.indexOf(note) >= 0 }
  /** Creates a new context which extends this context with `note`. */
  extend (note :Note) :Context { return new Context(this.tracer, [note].concat(this.notes)) }
  /** Creates a new context which extends this context with the entirety `other`. */
  concat (other :Context) :Context {
    return new Context(this.tracer, other.notes.concat(this.notes)) }

  /** Peels off the end of a context up to and including `note`. */
  peel (note :Note) :Context {
    const nidx = this.indexOf(note)
    if (nidx < 0) {
      console.warn(`Peeled unknown note from context ${note} :: ${this.notes}`)
      return new Context(this.tracer)
    } else {
      this.tracer.trace(`-- peeled ${this.notes} > ${note} < ${this.notes.slice(nidx+1)}`)
      return new Context(this.tracer, this.notes.slice(nidx+1))
    }
  }

  /** Splits this context into the part after `note` and the part before. `note` itself is not
    * included. Recall that contexts list notes in reverse order, hence the `(post, pre)` return
    * order. If `note` is not in this context `undefined` is returned. */
  split (note :Note) :[Context,Context]|void {
    const nidx = this.indexOf(note)
    if (nidx < 0) return undefined
    return [new Context(this.tracer, this.notes.slice(0, nidx)),
            new Context(this.tracer, this.notes.slice(nidx+1))]
  }

  /** Looks up the assumption for `sym`. */
  assump (sym :Symbol) :Type|void {
    let assumps = collect(this.notes, note => note instanceof NAssump &&
                          note.sym === sym ? note : undefined)
    switch (assumps.length) {
    case 0: return undefined
    case 1: return assumps[0].tpe
    default: return new Error(`Multiple types for '${sym}': ${assumps}`)
    }
  }

  /** Looks up the solution for `ev` in `ctx`. */
  solution (ev :EVar) :Type|void {
    let sols = collect(this.notes, note => note instanceof NSol && note.ev === ev ? note : undefined)
    switch (sols.length) {
    case 0: return undefined
    case 1: return sols[0].tpe
    default: return new Error(`Multiple solutions for '${ev}': ${sols}`)
    }
  }


  /** Derives a subtyping relationship `tpeA <: tpeB` within this context.
    * @return the output context or a string describing an error. */
  subtype (tpeA :Type, tpeB :Type) :Context|string {
    // <:Unit :: Γ ⊢ 1 <: 1 ⊣ Γ
    if (tpeA.equals(tpeB)) return this // Γ

    // TODO: handle widening primitives? coercing sum cases to sum type?

    // <:Var :: Γ[α] ⊢ α <: α ⊣ Γ[α]
    if (tpeA instanceof UVar && tpeB instanceof UVar && tpeA.equals(tpeB)) return this // Γ

    // <:Exvar :: Γ[â] ⊢ â <: â ⊣ Γ[â]
    if (tpeA instanceof EVar && tpeB instanceof EVar && tpeA.equals(tpeB)) {
      return this.contains(tpeA) ? this : `Unbound existential '${tpeA}'` // Γ
    }

    // <:→ :: Γ ⊢ A1→A2 <: B1→B2 ⊣ ∆
    if (tpeA instanceof Arrow && tpeB instanceof Arrow) {
      const theta = this.subtype(tpeB.arg, tpeA.arg) // Γ ⊢ B1 <: A1 ⊣ Θ
      return typeof theta === 'string' ? theta :
        theta.subtype(tpeA.res.apply(theta), tpeB.res.apply(theta)) // Θ ⊢ [Θ]A2 <: [Θ]B2 ⊣ ∆
    }

    // <:∀L :: Γ ⊢ ∀α.A <: B ⊣ ∆
    if (tpeA instanceof Abs) {
      const eA = freshEVar("a")
      const eAMark = new NMark(eA)
      const subCtx = this.extend(eAMark).extend(eA) // Γ,▶â,â
      const deltaEtc = subCtx.subtype(tpeA.body.subst(eA, tpeA.uv), tpeB) // [â/α]A <: B ⊣ ∆,▶â,Θ
      return typeof deltaEtc === 'string' ? deltaEtc : deltaEtc.peel(eAMark) // ∆
    }

    // <:∀R :: Γ ⊢ A <: ∀α.B ⊣ ∆
    if (tpeB instanceof Abs) {
      const deltaEtc = this.extend(tpeB.uv).subtype(tpeA, tpeB.body) // Γ,α ⊢ A <: B ⊣ ∆,α,Θ
      return typeof deltaEtc === 'string' ? deltaEtc : deltaEtc.peel(tpeB.uv) // ∆
    }

    // <:InstantiateL :: Γ[â] ⊢ â <: A ⊣ ∆
    if (tpeA instanceof EVar && this.contains(tpeA) && !tpeB.containsFree(tpeA)) {
      this.tracer.trace(`- <:InstL ${tpeA} :=< ${tpeB}`)
      return this.instantiateL(tpeA, tpeB) // Γ[â] ⊢ â :=< A ⊣ ∆
    }

    // <:InstantiateR :: Γ[â] ⊢ A <: â ⊣ ∆
    if (tpeB instanceof EVar && this.contains(tpeB) && !tpeA.containsFree(tpeB)) {
      this.tracer.trace(`- <:InstR ${tpeA} :=< ${tpeB}`)
      return this.instantiateR(tpeA, tpeB) // Γ[â] ⊢ A <: â ⊣ ∆
    }

    return `Type mismatch: expected '${tpeB}', given: '${tpeA}'`
  }

  /** Instantiates `eA` such that `eA <: a` in this context.
    * @return the output context or a string describing an error. */
  instantiateL (eA :EVar, a :Type) :Context|string {
    // InstLSolve :: Γ,â,Γ′ ⊢ â :=< τ ⊣ Γ,â=τ,Γ′
    if (a.isMono && a.isWellFormed(this.peel(eA))) { // Γ ⊢ τ
      const splits = this.split(eA)
      if (!splits) return `Unable to split context on ${eA}: ${this}`
      const [postCtx, preCtx] = splits
      this.tracer.trace(`- InstLSolve ${eA} :=< ${a}`)
      return preCtx.extend(new NSol(eA, a)).concat(postCtx) // Γ,â=τ,Γ′
    }

    // InstLReach :: Γ[â][ĉ] ⊢ â :=< ĉ ⊣ Γ[â][ĉ=â]
    if (a instanceof EVar && this.peel(a).contains(eA)) {
      const splits = this.split(a)
      if (!splits) return `Unable to split context on ${a}: ${this}`
      const [postCtx, preCtx] = splits
      this.tracer.trace(`- InstLReach ${eA} :=< ${a}`)
      return preCtx.extend(new NSol(a, eA)).concat(postCtx) // Γ[â][ĉ=â]
    }

    // InstLArr :: Γ[â] ⊢ â :=< A1 → A2 ⊣ ∆
    if (a instanceof Arrow && this.contains(eA)) {
      const splits = this.split(eA)
      if (!splits) return `Unable to split context on ${eA}: ${this}`
      const [postCtx, preCtx] = splits
      const a1 = a.arg, a2 = a.res
      const eA1 = freshEVar("a₁"), eA2 = freshEVar("a₂")
      const a1ctx = preCtx.extend(eA2).extend(eA1).extend(new NSol(eA, new Arrow(eA1, eA2))).
        concat(postCtx)
      this.tracer.trace(`- InstLArr(1) ${a1} :=< ${eA1} in ${a1ctx}`)
      const theta = a1ctx.instantiateR(a1, eA1) // Γ[â₂,â₁,â=â₁→â2] ⊢ A1 :=< â₁ ⊣ Θ
      if (typeof theta === 'string') return theta
      this.tracer.trace(`- InstRArr(2) ${eA2} :=< ${a2.apply(theta)} in ${theta}`)
      return theta.instantiateL(eA2, a2.apply(theta)) // Θ ⊢ â₂ :=< [Θ]A2 ⊣ ∆
    }

    // InstLAllR :: Γ[â] ⊢ â :=< ∀β.B ⊣ ∆
    if (a instanceof Abs && this.contains(eA)) {
      const uB = a.uv, b = a.body
      this.tracer.trace(`- InstLAllR ${eA} :=< ${b} in ${this.extend(uB)}`)
      const deltaEtc = this.extend(uB).instantiateL(eA, b) // Γ[â],β ⊢ â :=< B ⊣ ∆,β,∆′
      return typeof deltaEtc === 'string' ? deltaEtc : deltaEtc.peel(uB) // ∆
    }

    return `Failed to instantiate '${eA}' to '${a}'`
  }

  /** Instantiates `eA` such that `a <: eA` in this context.
    * @return the output context or a string describing an error. */
  instantiateR (a :Type, eA :EVar) :Context|string {
    // InstRSolve :: Γ,â,Γ′ ⊢ τ :=< â ⊣ Γ,â=τ,Γ′
    if (a.isMono && a.isWellFormed(this.peel(eA))) { /* Γ ⊢ τ */
      const splits = this.split(eA)
      if (!splits) return `Unable to split context on ${eA}: ${this}`
      const [postCtx, preCtx] = splits
      this.tracer.trace(`- InstRSolve ${a} :=< ${eA}`)
      return preCtx.extend(new NSol(eA, a)).concat(postCtx) // Γ,â=τ,Γ′
    }

    // InstRReach :: Γ[â][ĉ] ⊢ ĉ :=< â ⊣ Γ[â][ĉ=â]
    if (a instanceof EVar && this.peel(a).contains(eA)) {
    // case eC :TEVar if (peel(ctx, eC) contains eA) =>
      const splits = this.split(a)
      if (!splits) return `Unable to split context on ${a}: ${this}`
      const [postCtx, preCtx] = splits
      this.tracer.trace(`- InstRReach ${a} :=< ${eA}`)
      return preCtx.extend(new NSol(a, eA)).concat(postCtx) // Γ[â][ĉ = â]
    }

    // InstRArr :: Γ[â] ⊢ A1 → A2 :=< â ⊣ ∆
    if (a instanceof Arrow && this.contains(eA)) {
      const splits = this.split(eA)
      if (!splits) return `Unable to split context on ${eA}: ${this}`
      const [postCtx, preCtx] = splits
      const a1 = a.arg, a2 = a.res
      const eA1 = freshEVar("a₁"), eA2 = freshEVar("a₂")
      const a1ctx = preCtx.extend(eA2).extend(eA1).extend(new NSol(eA, new Arrow(eA1, eA2))).
        concat(postCtx)
      this.tracer.trace(`- InstRArr(1) ${eA1} :=< ${a1} in ${a1ctx}`)
      const theta = a1ctx.instantiateL(eA1, a1) // Γ[â₂,â₁,â=â₁→â₂] ⊢ â₁ :=< A1 ⊣ Θ
      if (typeof theta === 'string') return theta
      this.tracer.trace(`- InstRArr(2) ${a2.apply(theta)} :=< ${eA2} in ${theta}`)
      return theta.instantiateR(a2.apply(theta), eA2) // Θ ⊢ [Θ]A2 :=< â₂ ⊣ ∆
    }

    // InstRAllL :: Γ[â],▶ĉ,ĉ ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
    if (a instanceof Abs && this.contains(eA)) {
      const uB = a.uv, b = a.body
      const eC = freshEVar("c")
      const mC = new NMark(eC)
      const instCtx = this.extend(mC).extend(eC) // Γ[â],▶ĉ,ĉ
      this.tracer.trace(`- InstRAllL [${eC}/${uB}]${b} :=< ${eA} in ${instCtx}`)
      const deltaEtc = instCtx.instantiateR(b.subst(eC, uB), eA) // Γic ⊢ [ĉ/β]B :=< â ⊣ ∆,▶ĉ,∆′
      return typeof deltaEtc === 'string' ? deltaEtc : deltaEtc.peel(mC) // ∆
    }

    return `Failed to instantiate '${a}' to '${eA}'\n (context: ${this})`
  }

  toString () {
    return this.notes.toString()
  }
}

export abstract class Type {
  abstract get kind () :Kind
  get isError () :boolean { return false }
  get isArrow () :boolean { return false }
  get arity () :number { return 0 }

  /** Whether this type is a monotype. */
  get isMono () :boolean { return true }
    /** Returns whether `eV` is in the free variables of this type. */
  abstract containsFree (ev :EVar) :boolean

  /** Checks that this type is well-formed with respect to `ctx`, throws exception if not. */
  checkWellFormed (ctx :Context) {
    const err = this.checkMalformed(ctx)
    if (err !== undefined) throw new Error(err)
  }
  /** Returns whether this type is well-formed with respect to `ctx`. */
  isWellFormed (ctx :Context) :boolean { return this.checkMalformed(ctx) === undefined }
  /** Checks that this type is malformed with respect to `ctx`.
    * @return an error string if it is malformed, `undefined` if it is well-formed. */
  abstract checkMalformed (ctx :Context) :string|void

  /** Applies `ctx` to this type (substituting existential vars for their solutions). */
  apply (ctx :Context) :Type { return this }

  /** Returns this type with `thatT` replaced by `thisT`. */
  subst (thisT :EVar, thatT :UVar) :Type { return this }

  /** Infers the type of an application of this type to `tree`.
    * @return the inferred type and the output context. */
  inferApp (tree :Tree, ctx :Context) :[Type, Context] {
    return [new Error(`Cannot apply term of type '${this}' to '${tree}'`), ctx]
  }

  // abstract subsumes (that :Type) :boolean
  abstract equals (that :Type) :boolean

  // join (that :Type) :Type {
  //   if (this.equals(that)) return this
  //   else if (that instanceof Hole) return this
  //   else if (that instanceof Error) return this
  //   else if (that instanceof Def) return that.join(this)
  //   else return this._joinFailure(that)
  // }
  // protected _joinFailure (that :Type) :Type {
  //   return new Error(`Cannot join '${this}' with '${that}'`)
  // }
}

abstract class GroundType extends Type {
  get kind () { return star }
  containsFree (ev :EVar) :boolean { return false }
  checkMalformed (ctx :Context) :string|void { return undefined }
}

export class Hole extends GroundType {
  constructor () { super() }

  equals (that :Type) :boolean {
    return that instanceof Hole && this.kind.equals(that.kind)
  }
  // subsumes (that :Type) :boolean { return false }
  // join (that :Type) :Type { return that }
  // TODO: correct?
  // subsumes (that :Type) :boolean { return true }
  toString () { return `Hole` }
}
export const hole = new Hole()

export class Error extends GroundType {
  constructor (readonly msg :string) { super() }
  get isError () :boolean { return true }
  equals (that :Type) :boolean { return false }
  // join (that :Type) :Type { return that }
  // subsumes (that :Type) :boolean { return false }
  toString () { return `!!${this.msg}!!` }
}

export class Const extends GroundType {
  constructor (readonly cnst :Constant) { super() }
  equals (that :Type) :boolean { return that instanceof Const && this.cnst.equals(that.cnst) }
  // join (that :Type) :Type {
  //   if (that instanceof Const) {
  //     if (this.cnst.tag != that.cnst.tag) return super.join(that)
  //     else if (this.cnst.value === that.cnst.value) return this
  //     else return new Scalar(this.cnst.tag, Math.max(this.cnst.bitWidth, that.cnst.bitWidth))
  //   }
  //   else return that.join(this)
  // }
  // subsumes (that :Type) :boolean {
  //   if (that instanceof Const) {
  //     return (this.cnst.tag === that.cnst.tag &&
  //             this.cnst.value === that.cnst.value)
  //   }
  //   return false
  // }
  toString () { return `'${this.cnst.value}`}
}

export class UVar extends Type implements Note {
  get kind () { return star }
  constructor (readonly sym :Symbol) { super() }

  containsFree (ev :EVar) :boolean { return false }
  checkMalformed (ctx :Context) :string|void {
    if (!ctx.contains(this)) return `Unbound type variable '${name}'`
  }

  subst (thisT :EVar, thatT :UVar) :Type { return thatT.equals(this) ? thisT : this }

  equals (that :Type) :boolean { return that instanceof UVar && that.sym === this.sym }
  toString () { return `${this.sym.name}` }
}

export class EVar extends Type implements Note {
  get kind () { return star }
  constructor (readonly name :string) { super() }

  containsFree (ev :EVar) :boolean { return ev === this }
  checkMalformed (ctx :Context) :string|void {
    if (!ctx.contains(this) && !ctx.solution(this)) return `Unbound existential variable '${name}'`
  }

  apply (ctx :Context) :Type {
    const sol = ctx.solution(this)
    return sol ? sol.apply(ctx) : this
  }

  inferApp (tree :Tree, ctx :Context) :[Type, Context] { // âApp
    const a1 = freshEVar("a₁") // â₁
    const a2 = freshEVar("a₂") // â₂
    const aArrow = new Arrow(a1, a2) // â₁→â₂
    const splits = ctx.split(this)
    if (!splits) return [new Error(`Unable to split context on ${this}: ${ctx}`), ctx]
    const [postCtx, preCtx] = splits // Γpre[â]post
    const checkCtx = preCtx.extend(a2).extend(a1).extend(new NSol(this, aArrow)).
      concat(postCtx) // Γpre[â₂,â₁,â=â₁→â₂]post
    ctx.tracer.trace(`- âApp ${tree} <= ${a1} in ${checkCtx}`)
    return check(tree, checkCtx, a1, a2) // â₂ ⊣ ∆
  }

  equals (that :Type) :boolean { return that === this }
  toString () { return `#${this.name}` }
}

export class Abs extends Type {
  get kind () { return new KArrow(star, this.body.kind) }
  get arity () :number { return this.body.arity }

  readonly uv :UVar
  constructor (readonly sym :Symbol, readonly body :Type) { super() ; this.uv = new UVar(sym) }

  get isMono () :boolean { return false }
  containsFree (ev :EVar) :boolean { return this.body.containsFree(ev) }
  checkMalformed (ctx :Context) :string|void {
    return this.body.checkMalformed(ctx.extend(this.uv)) }

  apply (ctx :Context) :Type { return new Abs(this.sym, this.body.apply(ctx)) }
  subst (thisT :EVar, thatT :UVar) :Type { return new Abs(this.sym, this.body.subst(thisT, thatT)) }

  inferApp (tree :Tree, ctx :Context) :[Type, Context] { // ∀App
    // case TAll(uv, tpe) =>
    const eA = freshEVar("a") // â
    const reduced = this.body.subst(eA, this.uv) // [â/α]A
    const appCtx = ctx.extend(eA) // Γ,â
    ctx.tracer.trace(`- ∀App ${reduced} ● ${this} in ${appCtx}`)
    return reduced.inferApp(tree, appCtx) // C ⊣ ∆
  }

  get isArrow () :boolean { return this.body.isArrow }
  equals (that :Type) :boolean {
    // TODO: can the symbols be equal but the body not equal?
    return that instanceof Abs && this.sym === that.sym && this.body.equals(that.body)
  }
  // TODO: can we join type abstractions?
  // TODO: does abs subsume anything
  // subsumes (that :Type) :boolean { return false }
  // TODO: how to unify under an abs? just ignore the abs?
  toString () { return `∀${this.sym.name} ${this.body}`}
}

export class Arrow extends Type {
  get kind () { return star }
  get isArrow () :boolean { return true }
  get arity () :number { return 1 + this.res.arity }

  constructor (readonly arg :Type, readonly res :Type) { super() }

  get isMono () :boolean { return this.arg.isMono && this.res.isMono }
  containsFree (ev :EVar) :boolean { return this.arg.containsFree(ev) || this.res.containsFree(ev) }
  checkMalformed (ctx :Context) :string|void {
    return this.arg.checkMalformed(ctx) || this.res.checkMalformed(ctx)
  }

  apply (ctx :Context) :Type { return new Arrow(this.arg.apply(ctx), this.res.apply(ctx)) }
  subst (thisT :EVar, thatT :UVar) :Type { return new Arrow(
    this.arg.subst(thisT, thatT), this.res.subst(thisT, thatT)) }

  inferApp (tree :Tree, ctx :Context) :[Type, Context] { // ->App
    // A→C
    return check(tree, ctx, this.arg, this.res) // C ⊣ ∆
  }

  equals (that :Type) :boolean {
    return that instanceof Arrow && this.arg.equals(that.arg) && this.res.equals(that.res)
  }
  // join (that :Type) :Type {
  //   return (that instanceof Arrow) ? new Arrow(this.arg.join(that.arg), this.res.join(that.res)) :
  //     super.join(that)
  // }
  // subsumes (that :Type) :boolean {
  //   // TODO: technically potentially need to skolemize here, etc
  //   if (that instanceof Arrow) {
  //     return this.arg.subsumes(that.arg) && that.res.subsumes(this.res)
  //   }
  //   return false
  // }

  toString () { return `${this.arg} → ${this.res}`}
}

export class App extends Type {
  get kind () { return kindApply(this.ctor.kind, this.arg.kind) }
  constructor (readonly ctor :Type, readonly arg :Type) { super () }

  // TODO
  containsFree (ev :EVar) :boolean { return false }
  checkMalformed (ctx :Context) :string|void { return undefined }

  equals (that :Type) :boolean {
    return that instanceof App && this.ctor.equals(that.ctor) && this.arg.equals(that.arg)
  }
  // join (that :Type) :Type {
  //   return (that instanceof App) ? new App(this.ctor.join(that.ctor), this.arg.join(that.arg)) :
  //     super.join(that)
  // }
  // subsumes (that :Type) :boolean {
  //   if (that instanceof App) return this.ctor.subsumes(that.ctor) && this.arg.subsumes(that.arg)
  //   else return false
  // }

  toString () { return `${this.ctor} ${this.arg}`}
}

export class Scalar extends GroundType {
  get kind () { return star }
  constructor (readonly tag :Tag, readonly size :number) { super () }
  // join (that :Type) :Type {
  //   if (that instanceof Scalar) {
  //     if (this.tag !== that.tag) return super.join(that)
  //     else return this.size > that.size ? this : that
  //   } else if (that instanceof Const) {
  //     if (this.tag !== that.cnst.tag) return super.join(that)
  //     // TODO: will tag dictate signed versus unsigned?
  //     else return this
  //   }
  //   else return super.join(that)
  // }
  // subsumes (that :Type) :boolean {
  //   if (that instanceof Const) {
  //     if (this.tag !== that.cnst.tag) return false
  //     // TODO: will tag dictate signed versus unsigned?
  //     else return true
  //   }
  //   // TODO: widenings, coercions, whatnot
  //   return that instanceof Scalar && this.tag == that.tag && this.size >= that.size
  // }
  equals (that :Type) :boolean {
    return that instanceof Scalar && this.tag === that.tag && this.size == that.size
  }
  toString () { return `Scalar:${this.tag}${this.size}`}
}

export class Prod extends Type {
  get kind () { return star }
  constructor (readonly operands :Type[]) { super() }

  // TODO: hrm...
  checkMalformed (ctx :Context) :string|void { return undefined }
  containsFree (ev :EVar) :boolean { return false }

  // subsumes (that :Type) :boolean { return false }
  equals (that :Type) :boolean {
    return that instanceof Prod && this.operands.every((op, ii) => op.equals(that.operands[ii]))
  }
  toString () { return this.operands.length === 0 ? "()" :
    this.operands.map(op => op.toString()).join(" + ") }
}

export class Sum extends Type {
  get kind () { return star }
  constructor (readonly cases :Type[]) { super() }

  // TODO: hrm...
  checkMalformed (ctx :Context) :string|void { return undefined }
  containsFree (ev :EVar) :boolean { return false }

  // subsumes (that :Type) :boolean { return false }
  equals (that :Type) :boolean {
    return that instanceof Sum && this.cases.every((c, ii) => c.equals(that.cases[ii]))
  }
  toString () { return this.cases.map(cs => cs.toString()).join(" | ") }
}

export class Nominal extends Type {
  get kind () { return this.bodyFn().kind }

  constructor (readonly sym :Symbol, readonly bodyFn :() => Type) { super() }

  // TODO: hrm...
  checkMalformed (ctx :Context) :string|void { return undefined }
  containsFree (ev :EVar) :boolean { return false }

  // subsumes (that :Type) :boolean { return false }
  equals (that :Type) :boolean {
    return that instanceof Nominal && this.sym === that.sym
  }
  toString () { return this.sym.name } // TODO: signature?
}

// -- | Array Type?
// -- | Interface Name Params Methods
// -- | Method Name Type
