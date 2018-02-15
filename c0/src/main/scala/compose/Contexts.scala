//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Contexts {
  import Scopes._
  import Symbols._

  /** Contains things we need to keep track of during indexing and attribution. */
  case class Context (owner :Symbol, scope :MutableScope) {

    /** Clones this context with a new owner. */
    def withOwner (owner :Symbol) = if (this.owner == owner) this else copy(owner = owner)

    /** Clones this context with a fresh nested scope. */
    def enterScope () :Context = copy(scope = scope.nestedScope)

    /** Clones this context with a fresh nested scope and new owner. */
    def enterScope (owner :Symbol) :Context = copy(owner = owner, scope = scope.nestedScope)
  }
}
