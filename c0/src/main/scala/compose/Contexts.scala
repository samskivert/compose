//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Contexts {
  import Names._
  import Scopes._
  import Symbols._
  import Trees._
  import Types._

  final val TypingDef = 1 << 0

  /** Contains things we need to keep track of during indexing and typing.
    *
    * @param owner the symbol that owns the scope (usually a fundef).
    * @param scope holds names visible in this scope. This is usually the scope associated with
    * the owner, but can be further nested (for blocks, match cases, etc.).
    * @param flags allows things to behave differently depending on where we are in the tree.
    * Code reader confusion++!
    * @param patternProto used to convey an expected type when typing match case patterns (wherein
    * the main proto is the expected type of the whole match expression).
    */
  case class Context (
    owner :Symbol,
    scope :Scope,
    flags :Int = 0,
    patternProto :Type = Untyped
  ) {
    /** Whether we're currently typing a definition, versus an expression that computes a value. */
    def typingDef = isSet(TypingDef)

    /** Clones this context with a new owner. */
    def withOwner (owner :Symbol) =
      if (this.owner == owner) this else copy(owner = owner, scope = owner.scope)

    /** Clones this context with a nested scope. */
    def withNestedScope (name :Name) = copy(scope = scope.nestedScope(name))

    /** Clones this context, turning on `flag`. */
    def withFlag (flag :Int) =
      if (isSet(flag)) this else copy(flags = flags | flag)

    /** Clones this context, turning off `flag`. */
    def withoutFlag (flag :Int) =
      if (isSet(flag)) copy(flags = flags & ~flag) else this

    /** Clones this context with a new pattern prototype. */
    def withPatternProto (proto :Type) =
      if (patternProto == proto) this else copy(patternProto = proto)

    /** Creates a type symbol owned by the context owner, with a newly nested scope and enters it
      * into the context's scope. */
    def defineType (name :TypeName, tree :DefTree) :TypeSymbol =
      scope.enter(new TreeTypeSymbol(owner, scope.nestedScope(name), name, tree))

    /** Creates a term symbol owned by the context owner, with a newly nested scope. */
    def createTerm (name :TermName, tree :Tree, sigFn :Tree => Type) :TermSymbol =
      new TreeTermSymbol(owner, scope.nestedScope(name), name, tree, sigFn)

    /** Creates a term symbol owned by the context owner, with a newly nested scope and enters it
      * into the context's scope. */
    def defineTerm (name :TermName, tree :Tree, sigFn :Tree => Type) :TermSymbol =
      scope.enter(createTerm(name, tree, sigFn))

    /** Creates a term symbol owned by the context owner, with a newly nested scope and enters it
      * into the context's scope. */
    def defineTerm (name :TermName, tree :DefTree) :TermSymbol =
      defineTerm(name, tree, _ => tree.sig)

    private def isSet (flag :Int) = (flags & flag) == flag
  }

  object Context {
    /** Creates a context owned by `owner` and using `owner`'s scope. */
    def apply (owner :Symbol) :Context = apply(owner, owner.scope)
  }

  /** Creates a context for a module named `name`. */
  def moduleContext (name :TermName) = Context(newModuleSymbol(name))

  /** Returns the type for (type) `ident` in the implied `ctx`. */
  def typeFor (ident :TypeName)(implicit ctx :Context) :Type =
    ctx.scope.lookup(ident).map(_.info) getOrElse Unknown(ident, ctx.scope.id)

  /** Returns the type for (term) `ident` in the implied `ctx`. */
  def typeFor (ident :TermName)(implicit ctx :Context) :Type =
    ctx.scope.lookup(ident).map(_.info) getOrElse Unknown(ident, ctx.scope.id)
}
