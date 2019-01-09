package compose

object Symbols {
  import Constants._
  import Names._
  import Scopes._

  enum Kind { case Term, Type, Module } ; import Kind._
  enum Flavor { case None, Cnst, Func, Ctor } ; import Flavor._

  /** Defines the API needed for symbols to register themselves with their module's index (when
    * created), and remove themselves (when destroyed). */
  trait Index {
    /** Returns a peristent id for use by a new definition symbol in a module.
      * The symbol created with this id must be `insert`ed before the next call to `nextSymId`. */
    def nextSymId :Int
    /** Inserts `sym` into this index. */
    def insert (sym :Symbol) :Unit
    /** Removes `sym` from this index. */
    def remove (sym :Symbol) :Unit
  }

  abstract class Symbol (
    /** Whether this symbol represents a term, type or module. */
    val kind :Kind,
    /** Ad-hoc further refinement of this symbol's kind. */
    val flavor :Flavor,
    /** This symbol's persistent id. Only non-`0` for symbols that refer to trees. */
    val id :Int,
    /** The human readable name for this symbol. */
    val name :Name
  ) {
    def owner :Symbol
    // val scope :Scope
    // val type :Type

    /** Returns the root of this symbol's ownership chain. For most symbols this will be a module
      * symbol, but some syms can have esoteric owners (like missing syms, holes, and consts). */
    def root :Symbol = {
      var sym :Symbol = this ; var owner = this.owner
      while (sym != owner) {
        sym = owner
        owner = sym.owner
      }
      sym
    }

    /** Whether or not this symbol is lexically scoped. Lexically scoped symbols will only ever be
      * referenced in their lexical scope. Non-lexically scoped symbols may be referenced anywhere
      * in their module (or in other modules if they are exported). */
    def lexical = owner.kind != Module

    def index :Index = owner.index

    def isHole :Boolean = false // TODO: name == ""

    def toString = s"$name#${if (lexical) "l" else "m"}$id"

    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  class MissingSym (kind :Kind, name :Name) extends Symbol(kind, None, 0, name) {
    // get displayName () :string { return `<missing: ${name}>` }
    // get type () { return hole }
    def owner = this
    def index :Index = throw new Error(s"Cannot index through $this")
  }

  class TermHoleSym extends Symbol(Term, None, 0, NoName) {
    // get displayName () :string { return "?" }
    // get type () { return hole }
    def owner = this // TODO: proper owner?
    def index = throw new Error(s"Cannot index through $this")
  }

  class TypeHoleSym extends Symbol(Type, None, 0, NoName) {
    // get displayName () :string { return "?" }
    // get type () { return hole }
    def owner = this // TODO: proper owner?
    def index = throw new Error(s"Cannot index through $this")
  }

  class TermConstSym (cnst :Constant) extends Symbol(Term, Cnst, 0, termName(cnst.value)) {
    // get type () { return new Const(this.cnst) }
    def owner = this // TODO: none?
    def index = throw new Error(s"Cannot index through $this")
  }

  class TypeConstSym (cnst :Constant) extends Symbol(Term, Cnst, 0, typeName(cnst.value)) {
    // get type () { return new Const(this.cnst) }
    def owner = this // TODO: none?
    def index = throw new Error(s"Cannot index through $this")
  }

  class EmptySym extends Symbol(Kind.Term, Flavor.None, 0, termName("<empty>")) {
    // get type () :Type { return hole }
    def owner = this
    def index = throw new Error(s"Cannot index through $this")
  }
  val emptySym :Symbol = new EmptySym()
}
