//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

/** Indexes trees: enters definitions into the symbol table. */
object Indexer {
  import Contexts._
  import Symbols._
  import Trees._
  import Typer._
  import Types._

  /** Indexes the top-level of `tree`. Creates symbols for type and fun defs, entering them into
    * the symbol table of `ctx`. This does not descend into fun bodies or nested blocks. Those are
    * indexed separately, prior to being typed. */
  def index (tree :Tree)(implicit ctx :Context) :Symbol = (new Accumulator[Symbol] {
    println(s"indexing $tree")
    def apply (sym :Symbol, tree :Tree)(implicit ctx :Context) = tree match {
      case ParamDef(name, bounds) =>
        val paramSym = ctx.owner.defineType(name)
        paramSym.initInfo(Var(name)) // TODO: bounded type variables
        ctx.scope.enter(paramSym)

      case ArgDef(docs, name, typ) =>
        val argSym = ctx.owner.defineTerm(name)
        argSym.initInfo(typeFor(typ)) // TODO: inference!
        ctx.scope.enter(argSym)

      case FieldDef(docs, name, typ) =>
        val fieldSym = ctx.owner.defineTerm(name)
        fieldSym.initInfo(typeFor(typ))
        ctx.scope.enter(fieldSym)

      case RecordDef(docs, name, params, fields) =>
        val recSym = ctx.owner.defineType(name)
        val recCtx = ctx.withOwner(recSym)
        val paramSyms = params map { param => apply(sym, param)(recCtx).asType }
        val fieldSyms = fields map { field => apply(sym, field)(recCtx).asTerm }
        // TODO: enter these field symbols into `ctx` instead of `recCtx`?
        // fields foreach { f => apply(f)(recCtx) }
        recSym.initInfoLazy(() => Record(
          name, paramSyms.map(_.info), fieldSyms.map(f => Field(f.name, f.info))))
        ctx.scope.enter(recSym)
        // TODO: create a term symbol for the ctor fun?

      case UnionDef(docs, name, params, cases) =>
        val unionSym = ctx.owner.defineType(name)
        // TODO: ditto here about retaining context...
        val unionCtx = ctx.withOwner(unionSym)
        val paramSyms = params map { param => apply(sym, param)(unionCtx).asType }
        val caseSyms = cases map { cse => apply(sym, cse)(unionCtx).asType }
        unionSym.initInfoLazy(() => Union(name, paramSyms.map(_.info), caseSyms.map(_.info)))
        ctx.scope.enter(unionSym)

      case FunDef(docs, name, params, args, result, body) =>
        val funSym = ctx.owner.defineTerm(name)
        val funCtx = ctx.withOwner(funSym)
        val paramSyms = params map { param => apply(sym, param)(funCtx).asType }
        val argSyms = args map { arg => apply(sym, arg)(funCtx).asTerm }
        val resultType = typeFor(result)(funCtx) // TODO: result type inference...
        funSym.initInfoLazy(() => Arrow(paramSyms.map(_.info), argSyms.map(_.info), resultType))
        ctx.scope.enter(funSym)

      case Binding(name, typ, value) =>
        // TODO: put context in let or var mode, so Binding can note mutability
        val bindSym = ctx.owner.defineTerm(name)
        bindSym.initInfo(typeFor(typ)) // TODO: inference!
        ctx.scope.enter(bindSym)
      case LetDef(bindings) => foldOver(sym, tree)
      case VarDef(bindings) => foldOver(sym, tree)

      case _    :DefTree => throw new Exception(s"Missing DefTree case for $tree")
      case _    :Block   => sym // do not descend into nested blocks
      case tree          => foldOver(sym, tree)
    }
  }).apply(NoTerm, tree)
}
