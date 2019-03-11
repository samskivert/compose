package object compose {

  def fail (msg :String) :Nothing = throw new AssertionError(msg)

  def unreachable (obj :AnyRef) = fail(s"unreachable: $obj")
}
