package compose

object Symbols {
  import Constants._
  import Names._
  import Scopes._
  import Trees._
  import Types._

  enum Kind { case Term, Type, Module }
  enum Flavor { case None, Func, Ctor }

  abstract class Symbol (val name :Name) {
    /** Whether this symbol represents a term, type or module. */
    def kind :Kind
    /** Ad-hoc further refinement of this symbol's kind. */
    def flavor :Flavor
    /** The type of this symbol. */
    def tpe :Type

    override def toString = s"#$name"
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  class LexicalSym (name :Name, val tree :SymTree, val kind :Kind) extends Symbol(name) {
    def flavor = Flavor.None
    def tpe = tree.symType
  }

  // TODO: DefSym?

  abstract class DetachedSym (name :Name) extends Symbol(name) {
    def id = 0
    def tpe = Hole0
  }

  class MissingSym (val kind :Kind, name :Name) extends DetachedSym(name) {
    def flavor = Flavor.None
  }

  abstract class HoleSym extends DetachedSym(NoName) {
    def flavor = Flavor.None
  }
  class TermHoleSym extends HoleSym {
    def kind = Kind.Term
  }
  class TypeHoleSym extends HoleSym {
    def kind = Kind.Type
  }

  /** A placeholder symbol used during parsing. It is replaced with a real symbol in the name
    * resolution pass. */
  class ParsedSym (name :Name) extends DetachedSym(name) {
    def kind = Kind.Term
    def flavor = Flavor.None
  }
}
