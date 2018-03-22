//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Contexts {
  import Scopes._
  import Symbols._
  import Names._
  import Types._

  final val TypingDef = 1 << 0
  final val TypingVar = 1 << 1

  /** Contains things we need to keep track of during indexing and attribution. */
  case class Context (owner :Symbol, flags :Int = 0) {

    /** Returns the scope of the owner of this context. */
    def scope :Scope = owner.scope

    /** Whether we're currently typing a definition, versus an expression that computes a value. */
    def typingDef = isSet(TypingDef)

    /** Whether we're currently typing a `var` (versus a `let`). */
    def typingVar = isSet(TypingVar)

    /** Clones this context with a new owner. */
    def withOwner (owner :Symbol) = if (this.owner == owner) this else copy(owner = owner)

    /** Clones this context, turning on `flag`. */
    def withFlag (flag :Int) = if (isSet(flag)) this else copy(flags = flags | flag)

    /** Clones this context, turning off `flag`. */
    def withoutFlag (flag :Int) = if (isSet(flag)) copy(flags = flags & ~flag) else this

    private def isSet (flag :Int) = (flags & flag) == flag
  }

  /** Creates a context for a module named `name`. */
  def moduleContext (name :TermName) = Context(newModuleSymbol(name))

  /** Returns the type for (type) `ident` in the implied `ctx`. */
  def typeFor (ident :TypeName)(implicit ctx :Context) :Type =
    ctx.scope.lookup(ident).map(_.info) getOrElse Unknown(ident)

  /** Returns the type for (term) `ident` in the implied `ctx`. */
  def typeFor (ident :TermName)(implicit ctx :Context) :Type =
    ctx.scope.lookup(ident).map(_.info) getOrElse Unknown(ident)
}
