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
    * the symbol table of `ctx`. This does not descend into fun bodies or nested blocks. Those
    * will be indexed on demand, prior to being typed. */
  def index (tree :Tree)(implicit ctx :Context) :Symbol = (new Accumulator[Symbol] {
    def apply (sym :Symbol, tree :Tree)(implicit ctx :Context) = tree match {
      case TypeArgDef(name, bounds) =>
        ctx.scope.enter(new TypeSymbol(ctx.owner, name) {
          val info = Var(name) // TODO: bounded type variables
        })

      case ArgDef(docs, name, typ) =>
        ctx.scope.enter(new TermSymbol(ctx.owner, name) {
          lazy val info = typ.map(typeFor) getOrElse Prims.None // TODO: inference!
        })

      case FieldDef(docs, name, typ) =>
        ctx.scope.enter(new TermSymbol(ctx.owner, name) {
          lazy val info = typeFor(typ)
        })

      case RecordDef(docs, name, params, fields) =>
        ctx.scope.enter(new TypeSymbol(ctx.owner, name) {
          // TODO: it seems problematic that we have to retain this context but the field symbols
          // have to close over it to type themselves properly...
          val recCtx = ctx.enterScope(this)
          val paramSyms = params map { param => apply(sym, param)(recCtx).asType }
          val fieldSyms = fields map { field => apply(sym, field)(recCtx).asTerm }
          // TODO: enter these field symbols into `ctx` instead of `recCtx`?
          // fields foreach { f => apply(f)(recCtx) }
          lazy val info = Record(
            name, paramSyms.map(_.info), fieldSyms.map(f => Field(f.name, f.info)))
        })
        // TODO: create a term symbol for the ctor fun

      case UnionDef(docs, name, params, cases) =>
        ctx.scope.enter(new TypeSymbol(ctx.owner, name) {
          // TODO: ditto here about retaining context...
          val unionCtx = ctx.enterScope(this)
          val paramSyms = params map { param => apply(sym, param)(unionCtx).asType }
          val caseSyms = cases map { cse => apply(sym, cse)(unionCtx).asType }
          lazy val info = Union(name, paramSyms.map(_.info), caseSyms.map(_.info))
        })

      case FunDef(docs, name, params, args, result, body) =>
        ctx.scope.enter(new TermSymbol(ctx.owner, name) {
          val funCtx = ctx.enterScope(this)
          val paramSyms = params map { param => apply(sym, param)(funCtx).asType }
          val argSyms = args map { arg => apply(sym, arg)(funCtx).asTerm }
          lazy val info = Arrow(paramSyms.map(_.info), argSyms.map(_.info),
                                // TODO: return type inference...
                                result.map(res => typeFor(res)(funCtx)) getOrElse Prims.None)
        })

      case Binding(name, typ, value) =>
        ctx.scope.enter(new TermSymbol(ctx.owner, name) {
          lazy val info = typ.map(typeFor) getOrElse Prims.None // TODO: inference!
        })
      // TODO: put context in let or var mode, then have Binding do the right thing?
      case LetDef(bindings) => foldOver(sym, tree)
      case VarDef(bindings) => foldOver(sym, tree)

      case _    :DefTree => throw new Exception(s"Missing DefTree case for $tree")
      case _    :Block   => sym // do not descend into nested blocks
      case tree          => foldOver(sym, tree)
    }
  }).apply(NoTerm, tree)
}
