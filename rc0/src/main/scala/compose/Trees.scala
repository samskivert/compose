package compose

import java.io.PrintWriter

object Trees {
  import Analysis._
  import Constants._
  import Names._
  import Symbols._
  import Types._

  sealed trait Tree {
    def treeType :Type
    def signature :Type
    def isHole = false

    /** Returns a tree with names resolved (via `scope`). */
    def resolve (scope :Scope) :Tree

    /** Formats the tree (roughly) back into textual syntax. */
    def format :String

    /** Peforms a pre-order fold over the tree (visits this node, then children). */
    def fold[Z] (z :Z)(f :(Z, Tree) => Z) :Z = (f(z, this) /: children)(f)

    def children :Seq[Tree] = Seq()

    def debugPrint (out :PrintWriter, in :String) :Unit = {
      out.println(s"$in$debugShow :: $treeType")
      debugPrintChildren(out, s"$in ")
    }
    def debugPrintChildren (out :PrintWriter, in :String) :Unit =
      children foreach { _.debugPrint(out, s"$in ") }
    def debugShow :String = s"$debugName${debugShowArgs.mkString(" ", " ", "")}"
    def debugShowArgs :Seq[Any] = Seq()
    def debugName :String = getClass.getName match {
      case name if (name startsWith "compose.Trees$") => name.substring(14)
      case name => name
    }

    override def toString = format
  }

  sealed trait TypeTree extends Tree {
    def treeType = signature
    override def resolve (scope :Scope) :TypeTree
  }

  sealed trait TermTree extends Tree {

    protected var assignedType :Type = Hole0 // TODO: Unknown or Unset?
    def treeType = assignedType
    def signature = Hole0
    override def resolve (scope :Scope) :TermTree

    /** Checks that this tree has type `tpe` with input context `ctx` and records it as the tree's
      * type.
      * @return the output context or an error. */
    def checkSave (ctx :Context, tpe :Type) :Either[Error, Context] = {
      ctx.tracer.trace(s"Tree.check $this :: $tpe")
      val res = check(ctx, tpe)
      res match {
        case Left(error) =>
          assignedType = error
          ctx.tracer.trace(s"< $this :: $error // ∆ = $ctx")
        case Right(delta) =>
          assignedType = tpe.apply(delta)
          ctx.tracer.trace(s"< $this :: $assignedType // ∆ = $delta")
      }
      res
    }

    def check (ctx :Context, tpe :Type) :Either[Error, Context] = tpe match {
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
        inferSave(ctx) flatMap { (expType, theta) => // Γ ⊢ e ⇒ A ⊣ Θ
          ctx.tracer.trace(s"- Sub ($this => $expType) ; [Θ]$expType <: [Θ]$tpe in $theta")
          subtype(theta, expType.apply(theta), tpe.apply(theta)) // Θ ⊢ [Θ]A <: [Θ]B ⊣ ∆
        }
    }

    /** Infers a type for this tree with input context `ctx` and records its as the tree's type. The
      * recorded type is not yet applied to the context until inference is complete for the entire
      * expression (which happens in `DefTree.infer`).
      * @return the inferred type and the output context. */
    def inferSave (ctx :Context) :Either[Error, (Type, Context)] = {
      ctx.tracer.trace(s"infer $this")
      val res = infer(ctx)
      res match {
        case Left(error) =>
          assignedType = error
          ctx.tracer.trace(s"> $this :: $error // ∆ = $ctx")
        case Right((tpe, delta)) =>
          assignedType = tpe.apply(delta)
          ctx.tracer.trace(s"> $this :: $assignedType // ∆ = $delta")
      }
      res
    }

    def infer (ctx :Context) :Either[Error, (Type, Context)]

    /** Applies this tree's type to the supplied `ctx` (subbing existential vars for their
      * solutions).
      * @return the supplied `ctx`, unmodified. */
    def applyContext (ctx :Context) :Unit = {
      children foreach {
        case term :TermTree => term.applyContext(ctx)
        case _ => // skippy
      }
      assignedType = assignedType.apply(ctx)
      ctx.tracer.trace(s">> $this :: $assignedType")
    }
  }

  // sealed abstract class PatTree extends TermTree {
  // }

  // empty trees
  case object EmptyTermTree extends TermTree {
    def resolve (scope :Scope) = this
    // def owner = emptySym
    // get scope () :S.Scope { return S.emptyScope }
    // link (parent :Tree, parentId :string) { /*NOOP!*/ }
    def format = "<empty>"
    def infer (ctx :Context) = Right((Hole0, ctx))
  }

  case object EmptyTypeTree extends TypeTree {
    // def owner = emptySym
    def signature = Hole0
    def resolve (scope :Scope) = this
    def format = "<empty>"
    // get scope () :S.Scope { return S.emptyScope }
    // link (parent :Tree, parentId :string) { /*NOOP!*/ }
  }

  // type trees
  case object THoleTree extends TypeTree {
    def signature = Hole0
    def resolve (scope :Scope) = this
    def format = "<hole>"
    override def isHole = true
  }
  case class TConstTree (cnst :Constant) extends TypeTree {
    def signature = Const(cnst)
    def format = cnst.value
    override def debugShowArgs = Seq(cnst)
    def resolve (scope :Scope) = this
  }
  case class UTRefTree (name :TypeName) extends TypeTree {
    def signature = ??? // should never be called
    def format = s"?$name"
    override def debugShowArgs = Seq(name)
    def resolve (scope :Scope) = TRefTree(scope.lookupType(name))
  }
  case class TRefTree (sym :TypeSym) extends TypeTree {
    def signature = sym.sig
    def format = sym.toString
    override def debugShowArgs = Seq(sym)
    def resolve (scope :Scope) = this
  }
  case class ArrowTree (arg :TypeTree, res :TypeTree) extends TypeTree {
    def signature = new Arrow(arg.signature, res.signature)
    def format = s"${arg.format} → ${res.format}"
    def resolve (scope :Scope) = ArrowTree(arg.resolve(scope), res.resolve(scope))
    override def children = Seq(arg, res)
  }
  case class TAppTree (ctor :TypeTree, arg :TypeTree) extends TypeTree {
    def signature = new App(ctor.signature, arg.signature)
    def format = s"${ctor.format} ${arg.format}"
    def resolve (scope :Scope) = TAppTree(ctor.resolve(scope), arg.resolve(scope))
    override def children = Seq(ctor, arg)
  }
  case class ProdTree (fields :Seq[TypeTree]) extends TypeTree {
    def signature = ???
    def format = fields.map(_.format).mkString(" + ")
    def resolve (scope :Scope) = ProdTree(fields.map(_.resolve(scope)))
    override def children = fields
  }
  case class SumTree (cases :Seq[TypeTree]) extends TypeTree {
    def signature = ???
    def format = cases.map(_.format).mkString(" | ")
    def resolve (scope :Scope) = ProdTree(cases.map(_.resolve(scope)))
    override def children = cases
  }
  case class FieldTree (name :TermName, tpe :TypeTree) extends TypeTree {
    def signature = ???
    def format = s"$name:${tpe.format}"
    override def debugShowArgs = Seq(name)
    def resolve (scope :Scope) = FieldTree(name, tpe.resolve(scope))
    override def children = Seq(tpe)
  }
  case class CtorTree (name :TypeName, prod :TypeTree) extends TypeTree {
    def signature = ???
    def format = s"$name ${prod.format}"
    override def debugShowArgs = Seq(name)
    def resolve (scope :Scope) = CtorTree(name, prod.resolve(scope))
    override def children = Seq(prod)
  }
  // case class PrimTree (primType :Type) extends TypeTree

  // trees that define local symbols
  // abstract class PatSymTree extends PatTree with SymTree {
  //   protected var symType :Type = Hole0
  // }
  abstract class TermSymTree extends TermTree with Symbols.TermSymTree {
    protected var _symType :Type = Hole0
    def symType = _symType
  }

  // // pattern trees
  // case object PHoleTree extends PatTree {
  //   def infer (ctx :Context) = (Hole0, ctx)
  // }
  // case class PLitTree (cnst :Constant) extends PatTree {
  //   def infer (ctx :Context) = ???
  // }
  // case class PBindTree (sym :TermSym) extends PatSymTree {
  //   def infer (ctx :Context) = ???
  // }
  // case class PDtorTree (ctor :TermSym) extends PatTree {
  //   def infer (ctx :Context) = ???
  // }
  // case class PAppTree (fun :PatTree, arg :PatTree) extends PatTree {
  //   def infer (ctx :Context) = ???
  // }

  // abstraction trees
  case class AbsTree (sym :LexicalTermSym, ann :TypeTree, body :TermTree) extends TermSymTree {
    override def signature = Arrow(ann.signature, body.signature)
    def format = body match {
      case _ :AbsTree => s"$sym:${ann.format} → ${body.format}"
      case a :AscTree => s"$sym:${ann.format} → ${a.ann.format} = ${a.body.format}"
      case _          => s"$sym:${ann.format} ⇒ ${body.format}"
    }
    override def debugShowArgs = Seq(sym)
    def resolve (scope :Scope) = sym.setTree(
      AbsTree(sym, ann.resolve(scope), body.resolve(sym.scope(scope))))
    override def children = Seq(ann, body)
    override def check (ctx :Context, tpe :Type) = tpe match {
      case Arrow(arg, res) =>
        assignedType = tpe // lambda types are not always synthesized, so we also assign abs
        _symType = arg      // trees a type during checking, ditto for the lambda arg sym
        val argAssump = NAssump(sym, arg) // x:A
        val checkCtx = ctx.extend(argAssump)
        ctx.tracer.trace(s"- ->I ($body <= $res) in $checkCtx")
        body.checkSave(checkCtx, res) map { // Γ,x:A ⊢ e ⇐ B ⊣ ∆,x:A,Θ
          checkedCtx => checkedCtx.peel(argAssump) } // ∆
      case _ => super.check(ctx, tpe)
    }
    // ->I=> :: λx.e
    def infer (ctx :Context) = {
      val eA = freshEVar("a") // â
      val eC = freshEVar("c") // ĉ
      val assump = NAssump(this.sym, eA) // x:â
      _symType = eA // propagate type to arg sym
      val checkCtx = ctx.extend(eA).extend(eC).extend(assump) // Γ,â,ĉ,x:â
      ctx.tracer.trace(s"- ->I=> ($body <= $eC) in $checkCtx")
      body.checkSave(checkCtx, eC) map {
        checkedCtx => (Arrow(eA, eC), checkedCtx.peel(assump)) } // e ⇐ ĉ ⊣ ∆,x:â,Θ
    }
  }

  case class AllTree (sym :LexicalTypeSym, body :TermTree) extends TermTree with TypeSymTree {
    def symSig :UVar = UVar(sym)
    override def signature = Abs(symSig, body.signature)
    def format = s"∀$sym => ${body.format}"
    override def debugShowArgs = Seq(sym)
    def resolve (scope :Scope) = sym.setTree(AllTree(sym, body.resolve(sym.scope(scope))))
    override def children = Seq(body)
    override def check (ctx :Context, tpe :Type) = body.checkSave(ctx, tpe)
    def infer (ctx :Context) = body.inferSave(ctx)
  }

  case class Bind (sym :LexicalTermSym, ann :TypeTree, body :TermTree) extends Symbols.TermSymTree {
    protected var _symType :Type = Hole0
    def symType = _symType
    def assignType (tpe :Type) = { _symType = tpe }
    def format = s"$sym:${ann.format} = ${body.format}"
    def resolve (scope :Scope) = sym.setTree(
      // TODO: is it OK that we're always doing a recursive bind?
      Bind(sym, ann.resolve(scope), body.resolve(sym.scope(scope))))
  }

  case class LetTree (bind :Bind, expr :TermTree) extends TermTree {
    def format = s"let ${bind.format} in ${expr.format}"
    def resolve (scope :Scope) = {
      val nbind = bind.resolve(scope)
      LetTree(nbind, expr.resolve(nbind.sym.scope(scope)))
    }
    override def children = Seq(expr)
    override def debugPrintChildren (out :PrintWriter, in :String) :Unit = {
      out.println(s"${in}Bind ${bind.sym} : ${bind.ann.format} :: ${bind.symType}")
      bind.body.debugPrint(out, s"$in ")
      super.debugPrintChildren(out, in)
    }
    // TODO: if we have a type annotation, use that when typing the body
    def infer (ctx :Context) = {
      val eB = freshEVar("b")
      val assump = new NAssump(bind.sym, eB)
      bind.body.checkSave(ctx.extend(eB).extend(assump), eB) flatMap { theta =>
        val expType = eB.apply(theta)
        ctx.tracer.trace(s"- Typed binding ${bind.format} :: $expType")
        bind.assignType(expType) // assign type to binding symbol
        val eC = freshEVar("c")
        val assump = new NAssump(bind.sym, expType)
        val checkCtx = theta.peel(assump).extend(eC).extend(assump)
        ctx.tracer.trace(s"- Let=> ($expr <= $eC) in $checkCtx")
        expr.checkSave(checkCtx, eC) map { checkedCtx =>
          (eC.apply(checkedCtx), checkedCtx.peel(assump))
        }
      }
    }
  }

  case class LetRecTree (binds :Seq[Bind], expr :TermTree) extends TermTree {
    def format = s"let ${binds.map(_.format).mkString(" ; ")} in ${expr.format}"
    def resolve (scope :Scope) = ???
    override def children = Seq(expr)
    def infer (ctx :Context) = ???
  }

  // expression trees
  case class LitTree (cnst :Constant) extends TermTree {
    def format = cnst.value
    override def debugShowArgs = Seq(cnst)
    def resolve (scope :Scope) = this
    def infer (ctx :Context) = Right((Const(cnst), ctx))
  }
  case class URefTree (name :TermName) extends TermTree {
    def format = s"?$name"
    override def debugShowArgs = Seq(name)
    def resolve (scope :Scope) = RefTree(scope.lookupTerm(name))
    def infer (ctx :Context) = ??? // should never be called...
  }
  // TODO: ref and xref? (former comes from our context, latter has type that we use as is)
  case class RefTree (sym :TermSym) extends TermTree {
    def format = sym.toString
    override def debugShowArgs = Seq(sym)
    def resolve (scope :Scope) = this
    def infer (ctx :Context) =
      if (!sym.isDefined) Left(UnboundTerm(sym))
      else Right((ctx.assump(this.sym) getOrElse sym.tpe, ctx)) // A ⊣ Γ
  }
  case class AscTree (ann :TypeTree, body :TermTree) extends TermTree {
    def format = s"(${body.format} : ${ann.format})"
    def resolve (scope :Scope) = AscTree(ann.resolve(scope), body.resolve(scope))
    override def children = Seq(ann, body)
    override def check (ctx :Context, tpe :Type) = body.checkSave(ctx, tpe)
    override def signature = ann.signature
    def infer (ctx :Context) = {
      val tpe = ann.signature
      // TODO: tpe.checkWellFormed
      body.checkSave(ctx, tpe) map { checkedCtx => (tpe, checkedCtx) } // A ⊣ ∆
    }
  }
  case object HoleTree extends TermTree {
    def format = "<hole>"
    def resolve (scope :Scope) = this
    def infer (ctx :Context) = Right((Hole0, ctx))
  }
  case class AppTree (fun :TermTree, arg :TermTree) extends TermTree {
    def format = s"${fun.format} ${arg.format}"
    def resolve (scope :Scope) = AppTree(fun.resolve(scope), arg.resolve(scope))
    override def children = Seq(fun, arg)
    // ->E :: (e1 e2)
    def infer (ctx :Context) = fun.inferSave(ctx) flatMap { // e1 ⇒ A ⊣ Θ
      (funType, theta) =>
      val reducedFun = funType.apply(theta) // [Θ]A
      ctx.tracer.trace(s"- ->E $fun => $funType ; $reducedFun ● $arg in $theta")
      reducedFun.inferApp(this.arg, theta) map { (resType, delta) => // C ⊣ ∆
        // TODO: I think this is handled by a recursive apply of the final context with all the
        // solutions
        // this.fun.sig = funType.apply(delta)
        (resType, delta)
      }
    }
  }
  case class IfTree (test :TermTree, texp :TermTree, fexp :TermTree) extends TermTree {
    def format = s"if ${test.format} then ${texp.format} else ${fexp.format}"
    def resolve (scope :Scope) = IfTree(
      test.resolve(scope), texp.resolve(scope), fexp.resolve(scope))
    override def children = Seq(test, texp, fexp)
    // If=> :: if test ifTrue else ifFalse
    def infer (ctx :Context) = {
      test.check(ctx, Builtins.boolType) flatMap { delta =>
        texp.inferSave(delta) flatMap { (trueType, theta) =>
          fexp.inferSave(theta) flatMap { (falseType, phi) =>
            ctx.tracer.trace(s"- If=> ($texp => $trueType) ; ($fexp => $falseType) in $phi")
            unify(trueType, falseType) map { utype => (utype, phi) }
          }
        }
      }
    }
  }
  // case class MatchTree (scrut :TermTree, cases :Seq[CaseTree]) extends TermTree {
  //   def infer (ctx :Context) = ???
  // }
  // case class CaseTree (pat :PatTree, body :TermTree) extends TermTree {
  //   def infer (ctx :Context) = ???
  // }

  // top-level def trees
  case class TermDefTree (sym :LexicalTermSym, body :TermTree) extends TermTree
      with Symbols.TermSymTree {
    def symType = body.signature // assignedType
    def format = body match {
      case AscTree(ann, body) => s"def ${sym} :: ${ann.format} = ${body.format}"
      case _                  => s"def ${sym} :: ${body.format}"
    }
    override def debugShowArgs = Seq(sym)
    def resolve (scope :Scope) :TermDefTree =
      sym.setTree(TermDefTree(sym, body.resolve(sym.scope(scope))))
    override def children = Seq(body)

    /** Checks and records a type for this definition and all its subtrees. The top-level
      * definition should have a valid signature against which the term will be checked. Sub-terms
      * may have their types inferred.
      * @return the type assigned to this tree. */
    def assignType (trace :Boolean = false) :Type = {
      val ctx = newCtx(trace)
      val sig = body.signature
      if (sig == Hole0) throw new Exception(s"Cannot check term with no signature: $this")
      ctx.tracer.trace(s"$this ${sym.name} // inferType :: $sig")
      // TODO: sig.checkWellFormed
      body.checkSave(ctx, sig) match {
        case Left(error) =>
          assignedType = error
        case Right(delta) =>  // A ⊣ ∆
          assignedType = sig
          // TODO: do we still need to apply delta here? only forall trees auto-apply
          applyContext(delta)
      }
      if (trace) println(s"$this :: $treeType\n--\n" +
                         // debugShow.join("\n") + "\n--\n" +
                         ctx.tracer.result.mkString("\n"))
      assignedType
    }

    def infer (ctx :Context) = body.inferSave(ctx)
  }

  case class TypeDefTree (sym :LexicalTypeSym, body :TypeTree) extends TypeTree with TypeSymTree {
    def symSig = signature
    def signature = body.signature
    def format = s"type ${sym} = ${body.format}"
    override def debugShowArgs = Seq(sym)
    def resolve (scope :Scope) :TypeDefTree =
      sym.setTree(TypeDefTree(sym, body.resolve(sym.scope(scope))))
    override def children = Seq(body)
  }
}
