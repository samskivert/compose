package compose

object Analysis {
  import Types._
  import Symbols._

  // type checking
  trait Note derives Eql {}
  case class NAssump (sym :TermSym, tpe :Type) extends Note {
    override def toString = s"$sym~$tpe"
  }
  case class NSol (ev :EVar, tpe :Type) extends Note {
    override def toString = s"$ev=$tpe"
  }
  case class NMark (ev :EVar) extends Note {
    override def toString = s"*$ev"
  }
  // UVar and EVar are also notes

  class Tracer (trace :Boolean) {
    private val msgs = Seq.newBuilder[String]
    def trace (msg :String) :Unit = if (trace) msgs += msg
    def result = msgs.result()
  }

  var nextEVar = 1
  def freshEVar (name :String) :EVar = try EVar(s"$name$nextEVar") finally nextEVar += 1

  class Context (val tracer :Tracer, val notes :List[Note]) {

    /** Returns whether this context contains `note`. */
    def contains (note :Note) = notes.contains(note)
    /** Creates a new context which extends this context with `note`. */
    def extend (note :Note) :Context = new Context(tracer, note :: notes)
    /** Creates a new context which extends this context with the entirety of `other`. */
    def concat (other :Context) :Context = new Context(tracer, other.notes ++ notes)

    /** Peels off the end of a context up to and including `note`. */
    def peel (note :Note) :Context = {
      val nidx = notes.indexOf(note)
      if (nidx < 0) {
        // tracer.trace(s"-- peeled unknown note from context $note :: $notes")
        new Context(tracer, Nil)
      } else {
        // tracer.trace(s"-- peeled $notes to $note => ${notes.drop(nidx+1)}")
        new Context(tracer, notes.drop(nidx+1))
      }
    }

    /** Splits this context into the part after `note` and the part before. `note` itself is not
      * included. Recall that contexts list notes in reverse order, hence the `(post, pre)` return
      * order. If `note` is not in this context `None` is returned. */
    def split (note :Note) :Option[(Context,Context)] = {
      val nidx = notes.indexOf(note)
      if (nidx < 0) return None
      else Some(new Context(tracer, notes.take(nidx)) -> new Context(tracer, notes.drop(nidx+1)))
    }

    /** Looks up the assumption for `sym`. */
    def assump (sym :TermSym) :Option[Type] = (notes.collect {
      case assump :NAssump if (assump.sym == sym) => assump
    }) match {
      case Nil          => None
      case List(assump) => Some(assump.tpe)
      case assumps      => Some(MultipleAssumps(sym, assumps))
    }

    /** Looks up the solution for `ev` in `ctx`. */
    def solution (ev :EVar) :Option[Type] = (notes.collect {
      case sol :NSol if (sol.ev == ev) => sol
    }) match {
      case Nil       => None
      case List(sol) => Some(sol.tpe)
      case sols      => Some(MultipleSols(ev, sols))
    }

    override def toString = notes.toString
  }

  def newCtx (trace :Boolean) :Context = new Context(new Tracer(trace), Nil)
}
