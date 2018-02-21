//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Contexts {
  import Scopes._
  import Symbols._
  import Names._

  /** Contains things we need to keep track of during indexing and attribution. */
  case class Context (owner :Symbol) {

    /** Returns the scope of the owner of this context. */
    def scope :Scope = owner.scope

    /** Clones this context with a new owner. */
    def withOwner (owner :Symbol) = if (this.owner == owner) this else copy(owner = owner)
  }

  /** Creates a context for a module named `name`. */
  def moduleContext (name :TermName) = new Context(newModuleSymbol(name))
}
