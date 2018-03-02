//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object Constants {

  final val VoidTag = 0
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
    lazy val minWidth :Int = tag match {
      case BoolTag  => 1
      case IntTag   => intWidth(value)
      case FloatTag => floatWidth(value)
      case CharTag  => 16 // TODO
      case _        => 0
    }
    def kind = if (tag == RawStringTag) StringTag else tag
    override def toString = tag match {
      case VoidTag => "<void>"
      case UnitTag => "()"
      case CharTag => s"'${value}'"
      case StringTag => '"' + value + '"'
      case RawStringTag => s"`${value}`'"
      case _ => value
    }
  }

  def intWidth (value :String) :Int = {
    // TODO: something non-broken for int constants larger than Long.MaxValue
    val longValue = value.toLong
    if (longValue > Int.MaxValue) 64
    else if (longValue > Short.MaxValue) 32
    else if (longValue > Byte.MaxValue) 16
    else 8
  }

  def floatWidth (value :String) :Int = {
    // TODO: something non-broken for float constants larger than Double.MaxValue
    val doubleValue = value.toDouble
    if (doubleValue > Float.MaxValue) 64
    else 32
  }
}
