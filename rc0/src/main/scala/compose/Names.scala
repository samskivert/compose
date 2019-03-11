package compose

import java.util.HashMap

object Names {

  abstract class Name derives Eql {
    def isTypeName :Boolean
    def isTermName :Boolean
    def toTypeName :TypeName
    def toTermName :TermName

    def startsWithLower (prefix :String) = toString.toLowerCase.startsWith(prefix)

    // names are interned and can be hashed/compared by reference
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  class TermName extends Name {
    override def isTypeName = false
    override def isTermName = true
    override lazy val toTypeName = new TypeName(this)
    override def toTermName = this
  }

  class TypeName (term :TermName) extends Name {
    override def isTypeName = true
    override def isTermName = false
    override def toTypeName = this
    override def toTermName = term

    override def toString = term.toString
  }

  class SimpleName (raw :String) extends TermName {
    override def toString = raw
  }

  private val names = new HashMap[String, SimpleName]()

  /** Creates a `TermName` from a `raw` string. */
  def termName (raw :String) :TermName = names.get(raw) match {
    case null =>
      assert(!raw.isEmpty, "Symbol must have non-empty name")
      val name = new SimpleName(raw)
      // poor man's interning: will fancy up later if needed
      names.put(raw, name)
      name
    case name => name
  }

  /** Creates a `TypeName` from a `raw` string. */
  def typeName (raw :String) :TypeName = termName(raw).toTypeName

  /** Returns the (term) name of the tuple type for `rank`. */
  def tupleName (rank :Int) :TermName = termName(s"Tuple$rank")

  /** A sentinel name used for things that have no name. */
  val NoName = termName("<noname>")

  /** A reserved name for ignoring parts of patterns. */
  val IgnoreName = termName("_")

  // magic built-in name for arrays... meh
  val ArrayName = typeName("Array")

  // hacky approach to having functions implement operators
  val TermSyns :Map[TermName, TermName] = {
    def syn (from :String, to :String) = (termName(from), termName(to))
    Map(syn("not",   "!"),
        syn("eq",    "=="),
        syn("notEq", "!="),
        // comparison ops
        syn("less",      "<"),
        syn("lessEq",    "<="),
        syn("greater",   ">"),
        syn("greaterEq", ">="),
        // arithmetic ops
        syn("add", "+"),
        syn("sub", "-"),
        syn("mul", "*"),
        syn("div", "/"),
        syn("mod", "%"),
        // boolean ops
        syn("conj", "&&"),
        syn("disj", "||"),
        )
  }
}
