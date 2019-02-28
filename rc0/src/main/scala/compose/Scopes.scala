package compose

// import scala.collection.mutable.Builder

object Scopes {
  import Names._
  import Symbols._

  abstract class Scope {
    def lookupTerm (name :Name) :Sym
    def lookupType (name :Name) :Sym

    // def getCompletions (pred :(sym :Sym) => Boolean, prefix :String) = {
    //   val syms = Seq.newBuilder[Sym]
    //   _addCompletions(pred, prefix.toLowerCase, syms)
    //   syms.result()
    // }

    // def _addCompletions (pred :(sym :Sym) => Boolean, prefix :String,
    //                      syms :Builder[Sym,Seq[Sym]]) :Unit

    // protected def isCompletion (prefix :String, sym :Sym) :Boolean =
    //   !sym.isHole && sym.name.startsWithLower(prefix)
  }

  class NestedScope (parent :Scope) extends Scope {
    def lookupTerm (name :Name) = parent.lookupTerm(name)
    def lookupType (name :Name) = parent.lookupType(name)
  }
  class LexicalTermScope (parent :Scope, sym :Sym) extends NestedScope(parent) {
    override def lookupTerm (name :Name) = if (name == sym.name) sym else super.lookupTerm(name)
  }
  class LexicalTypeScope (parent :Scope, sym :Sym) extends NestedScope(parent) {
    override def lookupType (name :Name) = if (name == sym.name) sym else super.lookupType(name)
  }

  val emptyScope :Scope = new Scope {
    def lookupTerm (name :Name) :Sym = new MissingSym(Sort.Term, name)
    def lookupType (name :Name) :Sym = new MissingSym(Sort.Type, name)
    // def _addCompletions (pred :(sym :Sym) => Boolean, prefix :String,
    //                      syms :Builder[Sym,Seq[Sym]]) = {}
    override def toString = "<empty>"
  }
}
