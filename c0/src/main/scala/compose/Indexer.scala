//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

/** Indexes trees: enters definitions into the symbol table. */
object Indexer {
  import Contexts._
  import Symbols._
  import Trees._
  import Types._

  /** Indexes the top-level of `tree`. Creates symbols for type and fun defs, entering them into
    * the symbol table of `ctx`. This does not descend into fun bodies or nested blocks. Those are
    * indexed separately, prior to being typed. */
  def index (tree :Tree)(implicit ctx :Context) :Symbol = (new Accumulator[Symbol] {
    def log (tree :Tree) = { println(s"indexing ${tree.id}#${tree}") ; tree }
    def apply (sym :Symbol, tree :Tree)(implicit ctx :Context) = tree match {
      case tree @ Param(name) =>
        ctx.owner.defineType(name, tree, Seq())

      case Constraint(name, params) =>
        // constraints refer to already defined names, which we want to propagate up
        ctx.scope.lookup(name)

      case tree @ ArgDef(docs, name, typ) =>
        ctx.owner.defineTerm(name, tree)

      case tree @ FieldDef(docs, name, typ) =>
        ctx.owner.defineTerm(name, tree)

      case tree @ RecordDef(docs, name, params, fields) =>
        val recSym = ctx.owner.defineType(name, tree, params)
        val recCtx = ctx.withOwner(recSym)
        params foreach { param => apply(sym, param)(recCtx) }
        fields foreach { field => apply(sym, field)(recCtx) }
        // also create a term symbol for the ctor fun
        ctx.scope.enter(new TermSymbol(ctx.owner, ctx.scope.nestedScope(name), name.toTermName) {
          lazy val info = ctorType(recSym.info)
          override def toString = s"$what $name ($tree)"
        })
        recSym

      case tree @ UnionDef(docs, name, params, cases) =>
        val unionSym = ctx.owner.defineType(name, tree, params)
        val unionCtx = ctx.withOwner(unionSym)
        params foreach { param => apply(sym, param)(unionCtx) }
        // enter the cases, and lift their symbols into the same scope as the union
        cases foreach { cse =>
          val caseSym = apply(sym, cse)(unionCtx)
          ctx.scope.enter(caseSym)
          val caseCtorSym = unionCtx.scope.lookup(caseSym.name.toTermName)
          ctx.scope.enter(caseCtorSym)
        }
        unionSym

      case tree @ FunDef(docs, name, params, csts, args, result, body) =>
        val funSym = ctx.owner.defineTerm(name, tree)
        val funCtx = ctx.withOwner(funSym)
        params foreach { param => apply(sym, param)(funCtx).asType }
        args foreach { arg => apply(sym, arg)(funCtx) }
        funSym

      case tree @ FaceDef(docs, name, params, parents, meths) =>
        // augment the method declarations with the interface type vars
        val faceSym = ctx.owner.defineType(name, tree, params)
        val faceCtx = ctx.withOwner(faceSym)
        params foreach { param => apply(sym, param)(faceCtx) }
        // enter the methods, and lift their symbols into the same scope as the interface
        meths foreach { meth => ctx.scope.enter(apply(sym, meth)(faceCtx)) }
        faceSym

      case tree @ ImplDef(docs, name, params, csts, face, binds) =>
        val implSym = ctx.owner.defineTerm(name, tree)
        val implCtx = ctx.withOwner(implSym)
        params foreach { param => apply(sym, param)(implCtx).asType }
        implSym

      case tree @ Binding(name, typ, value) =>
        // TODO: put context in let or var mode, so Binding can note mutability
        ctx.owner.defineTerm(name, tree)
      case LetDef(bindings) => foldOver(sym, tree)
      case VarDef(bindings) => foldOver(sym, tree)

      case _    :DefTree => throw new Exception(s"Missing DefTree case for $tree")
      case _    :Block   => sym // do not descend into nested blocks
      case tree          => foldOver(sym, tree)
    }
  }).apply(NoTerm, tree)
}
