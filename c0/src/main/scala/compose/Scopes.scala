//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

/** Defines a symbol table which can be nested to match scopes. Adapted from dotc's. */
object Scopes {
  import Names._
  import Symbols._

  private val MinHashedScopeSize = 8
  private val FillFactor = 0.7
  private val MaxRecursions = 512
  private var scopeId = 0

  /** Maps names to symbols for a scope.
    * Includes mappings from all outer scopes that contain this one. */
  class Scope private[Scopes] (parent :Scope, val name :Name, val nestingLevel :Int) {
    private var last :ScopeEntry = _
    private var size = 0
    private var buckets :Array[ScopeEntry] = null

    /** A unique integer identifier for this scope. */
    val id = { scopeId += 1 ; scopeId }

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

    /** Returns the type symbol with `name` in this (or a parent) scope, or `NoType`. */
    def lookup (name :TypeName) :TypeSymbol = lookup(name, _.isType).asType

    /** Returns the term symbol with `name` in this (or a parent) scope, or `NoTerm`. */
    def lookup (name :TermName) :TermSymbol = lookup(name, _.isTerm).asTerm

    /** Looks up a symbol named `name` matching `pred`. Returns `NoTerm` if no match. */
    def lookup (name :Name, pred :Symbol => Boolean) :Symbol = {
      var ent :ScopeEntry = null
      if (buckets != null) {
        val idx = name.hashCode & (buckets.length-1)
        ent = buckets(idx)
        while (ent != null && (ent.name != name || !pred(ent.sym))) ent = ent.tail
      } else {
        ent = last
        while (ent != null && (ent.name != name || !pred(ent.sym))) ent = ent.prev
      }
      // println(s"$this lookup $name => $ent")
      if (ent != null) ent.sym
      else if (parent != null) parent.lookup(name, pred)
      else NoTerm
    }

    // TODO: lookupAll that returns all overloads for a symbol (only for termname)

    /** Creates a new scope nested under this scope. */
    def nestedScope (name :Name) :Scope = new Scope(this, name, nestingLevel+1)

    /** Enters `sym` into this scope (via its intrinsic `name`). */
    def enter (sym :Symbol) :sym.type = enter(sym.name, sym)

    /** Enters `sym` into this scope under `name`. */
    def enter (name :Name, sym :Symbol) :sym.type = {
      ensureCapacity(if (buckets != null) buckets.length else MinHashedScopeSize)
      val ent = new ScopeEntry(this, name, sym)
      ent.prev = last
      last = ent
      if (buckets != null) enterInHash(ent)
      // new Exception(s"$this entered: $name => $sym").printStackTrace(System.out)
      // println(s"$this entered: $name => $sym")
      size += 1
      // if this symbol has a synonym, enter it under that name as well
      Synonyms.get(name).map(syn => enter(syn, sym))
      sym
    }

    override def toString = s"Scope($name, $id, $nestingLevel)"
  }

  /** Creates a new top-level scope. */
  def newScope (name :Name) :Scope = new Scope(null, name, 0)

  /** Creates an empty read-only scope. Read-only scopes are used by `NoTerm` and `NoType` to
    * prevent nested symbols from unintentionally being entered into their scopes. */
  def newReadOnlyScope (name :Name) :Scope = new Scope(null, name, 0) {
    override def enter (name :Name, sym :Symbol) =
      throw new UnsupportedOperationException("Cannot enter symbol in read-only scope.")
  }

  private class ScopeEntry (val owner :Scope, val name :Name, _sym :Symbol) {
    var sym :Symbol = _sym
    var tail :ScopeEntry = null
    var prev :ScopeEntry = null
    override def toString = s"$name :: $sym"
  }
}
