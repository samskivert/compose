//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter

object Printing {

  type Brackets = (String, String)
  val Square = ("[", "]")
  val Paren = ("(", ")")
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
