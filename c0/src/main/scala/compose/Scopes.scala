//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

/** Defines a symbol table which can be nested to match scopes. Adapted from dotc's. */
object Scopes {
  import Names._
  import Symbols._

  /** Maps names to symbols for a scope.
    * Includes mappings from all outer scopes that contain this one. */
  abstract class Scope {

    /** Returns the type symbol with `name` in this (or a parent) scope, or `NoType`. */
    def lookup (name :TypeName) :TypeSymbol = lookup(name, _.isType).asType

    /** Returns the term symbol with `name` in this (or a parent) scope, or `NoTerm`. */
    def lookup (name :TermName) :TermSymbol = lookup(name, _.isTerm).asTerm

    /** Looks up a symbol named `name` matching `pred`. Returns `NoTerm` if no match. */
    def lookup (name :Name, pred :Symbol => Boolean) :Symbol

    // TODO: lookupAll that returns all overloads for a symbol (only for termname)

    /** Creates a new scope nested under this scope. */
    def nestedScope () :MutableScope
  }

  private val MinHashedScopeSize = 8
  private val FillFactor = 0.7
  private val MaxRecursions = 512

  // TODO: track outer scopes explicitly: we want nested scopes to see names entered after the
  // nested scope was created

  /** A scope into which mappings may be entered. */
  class MutableScope private[Scopes] (initLast :ScopeEntry, initSize :Int, val nestingLevel :Int)
      extends Scope {
    private var last :ScopeEntry = initLast
    private var size = initSize
    private var buckets :Array[ScopeEntry] = null

    private def ensureCapacity (tableSize :Int) :Unit =
      if (size >= tableSize * FillFactor) createHash(tableSize*2)

    private def createHash (tableSize :Int) :Unit =
      if (size > tableSize * FillFactor) createHash(tableSize*2)
      else {
        buckets = new Array[ScopeEntry](tableSize)
        enterAllInHash(last, 0)
      }

    private def enterInHash (ent :ScopeEntry) :Unit = {
      val idx = ent.name.hashCode & (buckets.length-1)
      ent.tail = buckets(idx)
      buckets(idx) = ent
    }

    // this needs to recapitulate the original order of entry, so we must trace our linked list to
    // its start and enter from there; we use recursion as much as possible but fall back to heap
    // allocations at max iters to avoid blowing the stack
    private def enterAllInHash (from :ScopeEntry, iter :Int) :Unit = if (from != null) {
      if (iter < MaxRecursions) {
        enterAllInHash(from.prev, iter+1)
        enterInHash(from)
      } else {
        var ents :List[ScopeEntry] = Nil
        var ent = from ; while (ent != null) {
          ents = ent :: ents
          ent = ent.prev
        }
        ents foreach enterInHash
      }
    }

    override def lookup (name :Name, pred :Symbol => Boolean) :Symbol = {
      if (buckets != null) {
        val idx = name.hashCode & (buckets.length-1)
        var ent = buckets(idx)
        while (ent != null && ent.name != name && !pred(ent.sym)) ent = ent.tail
        if (ent == null) NoTerm else ent.sym
      } else {
        var ent = last
        while (ent != null && ent.name != name && !pred(ent.sym)) ent = ent.prev
        if (ent == null) NoTerm else ent.sym
      }
    }

    override def nestedScope () :MutableScope = {
      val scope = new MutableScope(last, size, nestingLevel+1)
      scope.ensureCapacity(MinHashedScopeSize)
      scope
    }

    /** Enters `sym` into this scope (via its intrinsic `name`). */
    def enter (sym :Symbol) :sym.type = enter(sym.name, sym)

    /** Enters `sym` into this scope under `name`. */
    def enter (name :Name, sym :Symbol) :sym.type = {
      ensureCapacity(if (buckets != null) buckets.length else MinHashedScopeSize)
      val ent = new ScopeEntry(this, name, sym)
      ent.prev = last
      last = ent
      if (buckets != null) enterInHash(ent)
      size += 1
      sym
    }
  }

  private class ScopeEntry (val owner :Scope, val name :Name, _sym :Symbol) {
    var sym :Symbol = _sym
    var tail :ScopeEntry = null
    var prev :ScopeEntry = null
  }

  /** Creates a new top-level mutable scope. */
  def newScope :MutableScope = new MutableScope(null, 0, 0)
}
