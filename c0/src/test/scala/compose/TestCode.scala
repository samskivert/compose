//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

import java.nio.file.{Paths, Files}
import fastparse.core.Parsed
import fastparse.all.P

object TestCode {
  import Parser._

  def parseCode[T] (path :String, parser :P[T] = program) :Parsed[T, _, _] = {
    val cwd = Paths.get(System.getProperty("user.dir"))
    val fullPath = cwd.resolve("tests").resolve(path)
    val sb = new java.lang.StringBuilder
    Files.lines(fullPath).forEach(sb.append(_).append("\n"))
    parser.parse(sb.toString)
  }
}
