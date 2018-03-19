//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.io.PrintWriter
import java.nio.file.{Paths, Files}
import fastparse.all.P
import fastparse.core.Parsed
import org.junit.Assert._

object TestCode {
  import Contexts._
  import Indexer._
  import Names._
  import Parser._
  import Trees._

  val out = new PrintWriter(System.out)

  def parseCode[T] (path :String, parser :P[T] = program) :Parsed[T, _, _] = {
    val cwd = Paths.get(System.getProperty("user.dir"))
    val fullPath = cwd.resolve("tests").resolve(path)
    val sb = new java.lang.StringBuilder
    Files.lines(fullPath).forEach(sb.append(_).append("\n"))
    parser.parse(sb.toString)
  }

  def extract[T] (result :Parsed[T, _, _]) :T = result.fold(
    (p, pos, extra) => {
      extra.traced.trace.split(" / " ).foreach(f => println(s"- $f"))
      fail(result.toString)
      ??? // unreachable
    },
    (res, pos) => res
  )

  def testContext (name :String) = moduleContext(termName(s"${name}.cz"))

  def parseAndType (module :String, code :String) :Seq[Tree] = {
    val trees = extract(program.parse(code))
    implicit val ctx = testContext(module)
    trees foreach index
    trees map { tree => tree.typed() }
  }
}
