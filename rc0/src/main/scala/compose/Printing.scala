package compose

import java.io.{PrintWriter, Writer}
import java.lang.StringBuilder

object Printing {

  type Brackets = (String, String)
  val Square = ("[", "]")
  val Paren = ("(", ")")
  val Curly = ("{", "}")
  val Blank = ("", "")

  class Printer (out :PrintWriter, val indent :String = "") {
    def nest = new Printer(out, indent + "  ")
    def print (value :Any) = { out.print(value) ; this }
    def print (v0 :Any, v1 :Any, rest :Any*) = {
      out.print(v0) ; out.print(v1) ; rest.foreach(out.print) ; this
    }
    def println () :this.type = { out.println() ; this }
    def println (values :Any*) :this.type = { values.foreach(out.print) ; println() ; this }
    def printIndent (values :Any*) = { out.print(indent) ; values.foreach(out.print) ; this }
  }

  val sysOut = new PrintWriter(System.out)
  val sysPrint = new Printer(sysOut)

  def printInto (target :StringBuilder) :Printer = new Printer(new PrintWriter(new Writer() {
    override def write (c :Int) = target.append(c.asInstanceOf[Char])
    override def write (cs :Array[Char]) = target.append(cs)
    override def write (cs :Array[Char], off :Int, len :Int) = target.append(cs, off, len)
    override def write (s :String) = target.append(s)
    override def write (s :String, off :Int, len :Int) = target.append(s, off, len)
    override def close () :Unit = {}
    override def flush () :Unit = {}
  }))

  def printSep[T] (ts :Seq[T], printT :T => Unit, brackets :Brackets, sep :String = ", ")
                  (implicit pr :Printer) = {
    var first = true
    pr.print(brackets._1)
    ts foreach { t =>
      if (first) first = false
      else pr.print(sep)
      printT(t)
    }
    pr.print(brackets._2)
    pr
  }
}
