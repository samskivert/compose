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

  private def tag[A] (msg :String, a :A) :A = { /*println(msg + ":" + a);*/ a }

  /** Indexes the top-level of `tree`. Creates symbols for type and fun defs, entering them into
    * the symbol table of `ctx`. This does not descend into fun bodies or nested blocks. Those are
    * indexed separately, prior to being typed. */
  def index (tree :Tree)(implicit ctx :Context) :Symbol = (new Accumulator[Symbol] {
    def apply (sym :Symbol, tree :Tree)(implicit ctx :Context) = tag(s"indexing", tree) match {
      case Param(name) =>
        val paramSym = ctx.owner.defineType(name)
        paramSym.initInfo(Var(name))

      case Constraint(name, params) =>
        // constraints refer to already defined names, which we want to propagate up
        ctx.scope.lookup(name)

      case ArgDef(docs, name, typ) =>
        val argSym = ctx.owner.defineTerm(name)
        argSym.initInfo(typeFor(typ)) // TODO: inference!

      case FieldDef(docs, name, typ) =>
        val fieldSym = ctx.owner.defineTerm(name)
        fieldSym.initInfo(typeFor(typ))

      case RecordDef(docs, name, params, fields) =>
        val recSym = ctx.owner.defineType(name)
        val recCtx = ctx.withOwner(recSym)
        val paramSyms = params map { param => apply(sym, param)(recCtx).asType }
        val fieldSyms = fields map { field => apply(sym, field)(recCtx).asTerm }
        recSym.initInfoLazy(() => Record(
          name, paramSyms.map(_.info), fieldSyms.map(f => Field(f.name, f.info))))
        // TODO: create a term symbol for the ctor fun?

      case UnionDef(docs, name, params, cases) =>
        val unionSym = ctx.owner.defineType(name)
        val unionCtx = ctx.withOwner(unionSym)
        val paramSyms = params map { param => apply(sym, param)(unionCtx).asType }
        val caseSyms = cases map { cse => apply(sym, cse)(unionCtx).asType }
        // lift the case symbols symbols into the same scope as the data definition
        caseSyms foreach { sym => ctx.scope.enter(sym) }
        unionSym.initInfoLazy(() => Union(name, paramSyms.map(_.info), caseSyms.map(_.info)))

      case FunDef(docs, name, params, args, result, body) =>
        val funSym = ctx.owner.defineTerm(name)
        val funCtx = ctx.withOwner(funSym)
        val paramSyms = params map { param => apply(sym, param)(funCtx).asType }
        val argSyms = args map { arg => apply(sym, arg)(funCtx).asTerm }
        val resultType = typeFor(result)(funCtx) // TODO: result type inference...
        funSym.initInfoLazy(() => Arrow(paramSyms.map(_.info), argSyms.map(_.info), resultType))

      case FaceDef(docs, name, params, parents, meths) =>
        val faceSym = ctx.owner.defineType(name)
        val faceCtx = ctx.withOwner(faceSym)
        val paramSyms = params map { param => apply(sym, param)(faceCtx).asType }
        // augment the method declarations with the interface type vars and parent constraints
        val preParams = params ++ parents
        val fullMeths = meths map { meth => meth.copy(params = preParams ++ meth.params) }
        val methSyms = fullMeths map { meth => apply(sym, meth)(faceCtx).asTerm }
        // lift the method symbols into the same scope as the interface
        methSyms foreach { sym => ctx.scope.enter(sym) }
        faceSym.initInfoLazy(() => Interface(
          name, paramSyms.map(_.info), methSyms.map(m => Method(m.name, m.info))))

      case ImplDef(docs, name, params, face, binds) =>
        val implSym = ctx.owner.defineTerm(name)
        val implCtx = ctx.withOwner(implSym)
        val paramSyms = params map { param => apply(sym, param)(implCtx).asType }
        // TEMP: impl is typed as function from constraints to record
        implSym.initInfoLazy(() => Arrow(paramSyms.map(_.info), Seq(), typeFor(face)(implCtx)))

      case Binding(name, typ, value) =>
        // TODO: put context in let or var mode, so Binding can note mutability
        val bindSym = ctx.owner.defineTerm(name)
        bindSym.initInfo(typeFor(typ)) // TODO: inference!
      case LetDef(bindings) => foldOver(sym, tree)
      case VarDef(bindings) => foldOver(sym, tree)

      case _    :DefTree => throw new Exception(s"Missing DefTree case for $tree")
      case _    :Block   => sym // do not descend into nested blocks
      case tree          => foldOver(sym, tree)
    }
  }).apply(NoTerm, tree)
}
