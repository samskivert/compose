//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Constants {

  final val NoTag = 0
  final val UnitTag = 1
  final val BoolTag = 2
  final val IntTag = 3
  final val FloatTag = 4
  final val CharTag = 5
  final val StringTag = 6
  final val RawStringTag = 7

  val Unit = Constant(UnitTag, "")
  val True = Constant(BoolTag, "true")
  val False = Constant(BoolTag, "false")
  def bool (value :String) = value match {
    case "true"  => True
    case "false" => False
  }
  def int (value :String) = Constant(IntTag, value)
  def float (value :String) = Constant(FloatTag, value)
  def char (value :String) = Constant(CharTag, value)
  def string (value :String) = Constant(StringTag, value)
  def rawString (value :String) = Constant(RawStringTag, value)

  case class Constant (tag :Int, value :String) {
    override def toString = tag match {
      case NoTag => "<none?>"
      case UnitTag => "()"
      case CharTag => s"'${value}'"
      case StringTag => '"' + value + '"'
      case RawStringTag => s"`${value}`'"
      case _ => value
    }
  }
}
