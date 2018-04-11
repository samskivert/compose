//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

/** Indexes trees: enters definitions into the symbol table. */
object Indexer {
  import Contexts._
  import Names._
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
        tree.index(ctx.defineType(name, tree))

      case tree @ Constraint(name, params) =>
        // combine the constraint and its parameters into a name (mostly for debugging clarity, but
        // a backend might choose to use the name as-is to name its dictionaries)
        val dictName = termName(name + params.map(p => "$" + p).mkString)
        tree.index(ctx.defineTerm(dictName, tree))

      case tree @ ArgDef(docs, name, typ) =>
        tree.index(ctx.defineTerm(name, tree))

      case tree @ FieldDef(docs, name, typ) =>
        tree.index(ctx.defineTerm(name, tree))

      case tree @ RecordDef(docs, name, params, fields) =>
        val recSym = tree.index(ctx.defineType(name, tree))
        val recCtx = ctx.withOwner(recSym)
        params foreach { param => apply(sym, param)(recCtx) }
        fields foreach { field => apply(sym, field)(recCtx) }
        // also create a term symbol for the ctor fun
        ctx.scope.enter(new TermSymbol(name.toTermName) {
          val owner = ctx.owner
          val scope = ctx.scope.nestedScope(name)
          lazy val info = ctorType(recSym.info)
          override def toString = s"$what $name ($tree)"
        })
        recSym

      case tree @ UnionDef(docs, name, params, cases) =>
        val unionSym = tree.index(ctx.defineType(name, tree))
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
        val funSym = tree.index(ctx.defineTerm(name, tree))
        val funCtx = ctx.withOwner(funSym)
        params foreach { param => apply(sym, param)(funCtx) }
        csts foreach { cst => apply(sym, cst)(funCtx) }
        args foreach { arg => apply(sym, arg)(funCtx) }
        funSym

      case tree @ FaceDef(docs, name, params, parents, meths) =>
        // augment the method declarations with the interface type vars
        val faceSym = tree.index(ctx.defineType(name, tree))
        val faceCtx = ctx.withOwner(faceSym)
        params foreach { param => apply(sym, param)(faceCtx) }
        // enter the methods, and lift their symbols into the same scope as the interface
        meths foreach { meth => ctx.scope.enter(apply(sym, meth)(faceCtx)) }
        faceSym

      case tree @ ImplDef(docs, name, params, csts, face, binds) =>
        val implSym = tree.index(ctx.defineTerm(name, tree))
        val implCtx = ctx.withOwner(implSym)
        params foreach { param => apply(sym, param)(implCtx).asType }
        csts foreach { cst => apply(sym, cst)(implCtx) }

        val faceName = rootTypeName(face)
        val faceSym = ctx.scope.lookup(faceName)
        if (faceSym.exists) ctx.scope.enterImpl(faceSym, implSym)
        else println(s"Cannot register impl with invalid interface '${faceName}': $faceSym")
        implSym

      case _ :DefTree => throw new Exception(s"Missing DefTree case for $tree")
      case _ :DefExpr => foldOver(sym, tree)
      case _          => sym // do not index non-defs
    }
  }).apply(NoTerm, tree)

  def enterCsts (csts :Seq[Constraint])(implicit ctx :Context) :Unit = {
    // enter a symbol for the interface represented by this constraint, as well as any parent
    // interfaces; these will be used to resolve use of the interface methods on the type
    // variable being constrained
    csts foreach { cstTree =>
      val dictSym = cstTree.sym.asTerm
      def enterCstImpl (tree :Constraint) :Unit = {
        val faceSym = ctx.scope.lookup(tree.name)
        if (faceSym.exists) {
          ctx.scope.enterImpl(faceSym, dictSym)
          faceSym.csts foreach enterCstImpl
        } else println(s"Missing constraint: ${tree.name}")
        // else: the constraint tree will be typed with Error
      }
      enterCstImpl(cstTree)
    }
  }
}
