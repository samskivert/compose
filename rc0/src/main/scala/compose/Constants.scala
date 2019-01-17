package compose

object Constants {

  final val VoidTag = 'v'
  final val UnitTag = 'u'
  final val BoolTag = 'b'
  final val IntTag = 'i'
  final val HexTag = 'x'
  final val FloatTag = 'f'
  final val CharTag = 'c'
  final val StringTag = 's'

  val Unit = Constant(UnitTag, "")
  val True = Constant(BoolTag, "true")
  val False = Constant(BoolTag, "false")
  def bool (value :String) = value match {
    case "true"  => True
    case "false" => False
  }
  def int (value :String) = Constant(IntTag, value)
  def hex (value :String) = Constant(HexTag, value)
  def float (value :String) = Constant(FloatTag, value)
  def char (value :String) = Constant(CharTag, value)
  def string (value :String) = Constant(StringTag, value)

  case class Constant (tag :Int, value :String) {
    lazy val minWidth :Int = tag match {
      case BoolTag  => 1
      case IntTag   => intWidth(value)
      case HexTag   => value.length / 4
      case FloatTag => floatWidth(value)
      case CharTag  => 16 // TODO
      case _        => 0
    }
    override def toString = tag match {
      case VoidTag   => "<void>"
      case UnitTag   => "()"
      case CharTag   => s"'${value}'"
      case StringTag => '"' + value + '"'
      case HexTag    => s"0x${value}"
      case _         => value
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
