package compose

// import scala.collection.mutable.Builder

object Scopes {
  import Names._
  import Symbols._

  abstract class Scope {
    def lookupTerm (name :Name) :Symbol = lookup(Sort.Term, name)
    def lookupType (name :Name) :Symbol = lookup(Sort.Type, name)

    def lookup (kind :Sort, name :Name) :Symbol

    // def getCompletions (pred :(sym :Symbol) => Boolean, prefix :String) = {
    //   val syms = Seq.newBuilder[Symbol]
    //   _addCompletions(pred, prefix.toLowerCase, syms)
    //   syms.result()
    // }

    // def _addCompletions (pred :(sym :Symbol) => Boolean, prefix :String,
    //                      syms :Builder[Symbol,Seq[Symbol]]) :Unit

    // protected def isCompletion (prefix :String, sym :Symbol) :Boolean =
    //   !sym.isHole && sym.name.startsWithLower(prefix)
  }

  val emptyScope :Scope = new Scope {
    def lookup (kind :Sort, name :Name) :Symbol = new MissingSym(kind, name)
    // def _addCompletions (pred :(sym :Symbol) => Boolean, prefix :String,
    //                      syms :Builder[Symbol,Seq[Symbol]]) = {}
    override def toString = "<empty>"
  }
}
