//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

/** Indexes trees: enters definitions into the symbol table. */
object Indexer {
  import Contexts._
  import Symbols._
  import Trees._

  /** Indexes the top-level of `tree`. Creates symbols for type and fun defs, entering them into
    * the symbol table of `ctx`. This does not descend into fun bodies or nested blocks. Those are
    * indexed separately, prior to being typed. */
  def index (tree :Tree)(implicit ctx :Context) :Symbol = (new Accumulator[Symbol] {
    def apply (sym :Symbol, tree :Tree)(implicit ctx :Context) = {
      // println(s"indexing ${tree.id}#${tree}")
      tree match {
      case Param(name) =>
        ctx.owner.defineType(name, tree, Seq())

      case Constraint(name, params) =>
        // constraints refer to already defined names, which we want to propagate up
        ctx.scope.lookup(name)

      case ArgDef(docs, name, typ) =>
        ctx.owner.defineTerm(name, tree)

      case FieldDef(docs, name, typ) =>
        ctx.owner.defineTerm(name, tree)

      case RecordDef(docs, name, params, fields) =>
        val recSym = ctx.owner.defineType(name, tree, params)
        val recCtx = ctx.withOwner(recSym)
        params foreach { param => apply(sym, param)(recCtx).asType }
        fields foreach { field => apply(sym, field)(recCtx).asTerm }
        // TODO: create a term symbol for the ctor fun?
        recSym

      case UnionDef(docs, name, params, cases) =>
        val unionSym = ctx.owner.defineType(name, tree, params)
        val unionCtx = ctx.withOwner(unionSym)
        params foreach { param => apply(sym, param)(unionCtx).asType }
        // enter the cases, and lift their symbols symbols into the same scope as the union
        cases map { cse => ctx.scope.enter(apply(sym, cse)(unionCtx).asType) }
        unionSym

      case FunDef(docs, name, params, args, result, body) =>
        val funSym = ctx.owner.defineTerm(name, tree)
        val funCtx = ctx.withOwner(funSym)
        // if we're a method, there will be 'ambient' parameters inherited from our enclosing
        // interface
        (ctx.owner.params ++ params) foreach { param => apply(sym, param)(funCtx).asType }
        args foreach { arg => apply(sym, arg)(funCtx).asTerm }
        funSym

      case FaceDef(docs, name, params, parents, meths) =>
        // augment the method declarations with the interface type vars and parent constraints
        val faceSym = ctx.owner.defineType(name, tree, params ++ parents)
        val faceCtx = ctx.withOwner(faceSym)
        params foreach { param => apply(sym, param)(faceCtx).asType }
        // enter the methods, and lift their symbols into the same scope as the interface
        meths map { meth => ctx.scope.enter(apply(sym, meth)(faceCtx).asTerm) }
        faceSym

      case ImplDef(docs, name, params, face, binds) =>
        val implSym = ctx.owner.defineTerm(name, tree)
        val implCtx = ctx.withOwner(implSym)
        params foreach { param => apply(sym, param)(implCtx).asType }
        implSym

      case Binding(name, typ, value) =>
        // TODO: put context in let or var mode, so Binding can note mutability
        ctx.owner.defineTerm(name, tree)
      case LetDef(bindings) => foldOver(sym, tree)
      case VarDef(bindings) => foldOver(sym, tree)

      case _    :DefTree => throw new Exception(s"Missing DefTree case for $tree")
      case _    :Block   => sym // do not descend into nested blocks
      case tree          => foldOver(sym, tree)
    }
    }
  }).apply(NoTerm, tree)
}
