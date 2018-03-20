//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.util.HashMap

object Names {

  abstract class Name {
    /** A type for methods that return `this`. */
    type ThisName <: Name

    def isTypeName :Boolean
    def isTermName :Boolean
    def toTypeName :TypeName
    def toTermName :TermName

    // names are interned and can be hashed/compared by reference
    override def hashCode = System.identityHashCode(this)
    override def equals (that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  class TermName extends Name {
    type ThisName = TermName

    override def isTypeName = false
    override def isTermName = true
    override lazy val toTypeName = new TypeName(this)
    override def toTermName = this
  }

  class TypeName (term :TermName) extends Name {
    type ThisName = TypeName

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

  // poor man's interning: will fancy up later if needed
  def termName (raw :String) :TermName = names.get(raw) match {
    case null =>
      assert(!raw.isEmpty, "Symbol must have non-empty name")
      val name = new SimpleName(raw)
      names.put(raw, name)
      name
    case name => name
  }
  def typeName (raw :String) = termName(raw).toTypeName

  val NoName = termName("<noname>")

  // TEMP: magic built-in name for arrays... meh
  val ArrayName = typeName("Array")

  // hacky approach to having functions implement operators
  val Synonyms :Map[Name, Name] = {
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
        )
  }
}
