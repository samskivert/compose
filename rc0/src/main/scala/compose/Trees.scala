package compose

import Constants._

object Trees {
  import Types.{Context, Type, Abs}

  sealed trait Tree {

  // get parent () :Tree|void { return this._parent }
  // get parentId () :string { return this._parentId }
  // get owner () :S.Symbol { return this._owner }
  // get scope () :S.Scope { return this._scope }
  // get kind () :string {
  //   const name = this.constructor.name
  //   return name.substring(0, name.length-4).toLowerCase() // trim the tree
  // }

  // get isHole () :boolean { return false }
  // /** The type described by this tree. */
  // get signature () :TP.Type { return TP.hole }
  // /** The type ascribed to this tree. */
  // abstract get treeType () :TP.Type

  }

  sealed abstract class TypeTree extends Tree {
  }

  sealed abstract class TermTree extends Tree {

    /** Checks that this tree has type `tpe` with input context `ctx`.
      * @return the output context or an error. */
    def check (ctx :Context, tpe :Type) :Either[String, Context] = {
      ctx.tracer.trace(s"Tree.check $this :: $tpe")
      tpe match {
        case abs @ Abs(_, body) =>
          // ∀I :: (e, ∀α.A)
          val checkCtx = ctx.extend(abs.uv)
          ctx.tracer.trace(s"- ∀I ($this <= $body) in $checkCtx")
          check(checkCtx, body) map { checkedCtx => // Γ,α ⊢ e ⇐ A ⊣ ∆,α,Θ
            this.applyContext(checkedCtx)
            checkedCtx.peel(abs.uv) // ∆
          }
        case _ =>
          // Sub :: (e, B)
          val (expType, theta) = inferSave(ctx) // Γ ⊢ e ⇒ A ⊣ Θ
          ctx.tracer.trace(s"- Sub ($this => $expType) ; [Θ]$expType <: [Θ]$tpe in $theta")
          theta.subtype(expType.apply(theta), tpe.apply(theta)) // Θ ⊢ [Θ]A <: [Θ]B ⊣ ∆
      }
    }

    /** Infers a type for this tree with input context `ctx` and records its as the tree's type. The
      * recorded type is not yet applied to the context until inference is complete for the entire
      * expression (which happens in `DefTree.infer`).
      * @return the inferred type and the output context. */
    def inferSave (ctx :Context) :(Type, Context) = {
      ctx.tracer.trace(s"infer $this")
      val (tpe, delta) = infer(ctx)
      // this.inferredType = tpe
      ctx.tracer.trace(s"> $this :: $tpe // ∆ = $delta")
      (tpe, delta)
    }

    protected def infer (ctx :Context) :(Type, Context)

    /** Applies this tree's type to the supplied `ctx` (subbing existential vars for their
      * solutions).
      * @return the supplied `ctx`, unmodified. */
    protected def applyContext (ctx :Context) :Unit = {
      // for (let id of this.branchIds) {
      //   const branch = this.branch(id)
      //   if (branch instanceof TermTree) branch.applyContext(ctx)
      // }
      // inferredType = inferredType.apply(ctx)
      // ctx.tracer.trace(s">> $this :: $inferredType")
    }
  }

  sealed abstract class PatTree extends TermTree {
  }

  sealed trait SymTree extends Tree {
    val sym :Symbol
  }

  // type trees
  case object THoleTree extends TypeTree
  case class TConstTree (cnst :Constant) extends TypeTree
  case class TRefTree (sym :Symbol) extends TypeTree
  case class ArrowTree (from :TypeTree, to :TypeTree) extends TypeTree
  case class TAppTree (ctor :TypeTree, arg :TypeTree) extends TypeTree
  case class ProdTree (fields :Seq[TypeTree]) extends TypeTree
  case class SumTree (cases :Seq[TypeTree]) extends TypeTree
  case class FieldTree (sym :Symbol, tpe :TypeTree) extends TypeTree with SymTree
  case class CtorTree (sym :Symbol, prod :TypeTree) extends TypeTree with SymTree
  // case class PrimTree (primType :Type) extends TypeTree

  // pattern trees
  case object PHoleTree extends PatTree
  case class PLitTree (cnst :Constant) extends PatTree
  case class PBindTree (sym :Symbol) extends PatTree with SymTree
  case class PDtorTree (ctor :Symbol) extends PatTree
  case class PAppTree (fun :PatTree, arg :PatTree) extends PatTree

  // abstraction trees
  case class LetTree (sym :Symbol, tpe :TypeTree, body :TermTree, expr :TermTree)
      extends TermTree with SymTree
  case class AllTree (sym :Symbol, body :TermTree) extends TermTree with SymTree
  case class AbsTree (sym :Symbol, tpe :TypeTree, body :TermTree) extends TermTree with SymTree

  // expression trees
  case class LitTree (cnst :Constant) extends TermTree
  case class RefTree (sym :Symbol) extends TermTree
  case class AscTree (tpe :TypeTree, body :TermTree) extends TermTree
  case object HoleTree extends TermTree
  case class AppTree (fun :TermTree, arg :TermTree) extends TermTree
  case class IfTree (test :TermTree, texp :TermTree, fexp :TermTree) extends TermTree
  case class MatchTree (scrut :TermTree, cases :Seq[CaseTree]) extends TermTree
  case class CaseTree (pat :PatTree, body :TermTree) extends TermTree

  // top-level def trees
  case class TermDefTree (sym :Symbol, body :TermTree) extends TermTree with SymTree
  case class TypeDefTree (sym :Symbol, body :TypeTree) extends TypeTree with SymTree
}
