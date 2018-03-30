//
// Compose Mark 0 compiler
// https://github.com/samskivert/compose/c0/LICENSE

package compose

object JSGen {
  import Constants._
  import Lower._
  import Names._
  import Printing._
  import compose.{Types => tpes}

  def gen (stmts :Seq[StmtTree], pr :Printer) :Unit = {
    implicit val npr = pr
    stmts foreach { stmt => genJS(stmt).println() }
  }

  private def genJS (tree :Tree)(implicit pr :Printer) :Printer = tree match {
    case Literal(const) => const.tag match {
      case VoidTag   => pr.print("undefined")
      case UnitTag   => pr.print("{}")
      case BoolTag   => pr.print(const.value)
      case IntTag    => pr.print(const.value) // TODO: syntax conversion?
      case FloatTag  => pr.print(const.value) // TODO: syntax conversion?
      case CharTag   => pr.print(s"'${const.value}'.charAt(0)")
      case StringTag => pr.print('"' + const.value + '"')
      case RawStrTag => pr.print(s"`${const.value}`'")
    }
    // case ArrayLiteral(values) =>
    //   pr.print(s"todo($tree)")
    case IdentRef(sym) =>
      genSym(sym)
    case Select(expr, field) =>
      genJS(expr).print(".") ; genSym(field)
    case Index(array, index) =>
      genJS(array).print("[") ; genJS(index).print("]")
    case Is(expr, what) =>
      // TODO: do the right thing for tagged union tests
      genJS(expr) ; pr.print(" === ") ; genJS(what)
    case And(left, right) =>
      pr.print("(") ; genJS(left) ; pr.print(" && ") ; genJS(right) ; pr.print(")")
    case Lambda(dicts, args, body) =>
      printSep(dicts ++ args, genSym, Paren).print(" => ") ; genJS(body)
    case Apply(fun, dicts, args) =>
      genJS(fun) ; printSep(dicts ++ args, genJS, Paren)
    case Construct (obj, args) =>
      // TODO: union tag
      val names = obj.sig match {
        case tpes.Interface(_, _, methods) => methods.map(_.sym.name)
        case tpes.Record(_, _, fields) => fields.map(_.sym.name)
        case tpe => unreachable(tpe)
      }
      def printProp (nameArg :(Name, ExprTree)) = { pr.print(nameArg._1, ": ") ; genJS(nameArg._2) }
      printSep(names zip args, printProp, Curly)
    case Block(stmts, labelOpt) =>
      labelOpt foreach { lbl => pr.print(s"L${lbl}: ") }
      pr.print("{")
      val npr = pr.nest
      stmts foreach { stmt => npr.println().printIndent() ; genJS(stmt)(npr) }
      pr.println().printIndent("}")
    case ExprStmt(expr) =>
      genJS(expr) ; pr.print(";")
    case Assign(ident, value) =>
      genSym(ident) ; pr.print(" = ") ; genJS(value) ; pr.print(";")
    case Return(value) =>
      pr.print("return ") ; genJS(value) ; pr.print(";")
    case Break(label) =>
      pr.print(s"break L${label};")
    case Raise(value) =>
      pr.print("throw ") ; genJS(value) ; pr.print(";")
    case If(cond, ifTrue) =>
      pr.print("if (") ; genJS(cond) ; pr.print(") ") ; genJS(ifTrue)
    case IfElse(cond, ifTrue, ifFalse) =>
      pr.print("if (") ; genJS(cond) ; pr.print(") ") ; genJS(ifTrue)
      if (isBlock(ifTrue)) pr.print(" else ") else pr.println().printIndent("else ")
      genJS(ifFalse)
    case While(cond, body) =>
      pr.print("while (") ; genJS(cond) ; pr.print(") ") ; genJS(body)
    case DoWhile(body, cond) =>
      pr.print("do ") ; genJS(body) ; pr.print(" while (") ; genJS(cond) ; pr.print(")")
    case Foreign(body) =>
      pr.print(body)

    case LetDef(ident, mut, valopt) =>
      pr.print(if (mut) "let " else "const ") ; genSym(ident)
      valopt.foreach(value => { pr.print(" = ") ; genJS(value) }) ; pr.print(";")
    case RecordDef(ident, fields) =>
      pr // TODO
    case UnionDef(ident, cases) =>
      pr // TODO
    case FunDef(ident, dicts, args, body) =>
      pr.print("function ") ; genSym(ident) ; pr.print(" ") ; printSep(dicts ++ args, genSym, Paren)
      pr.print(" ")
      if (!isBlock(body)) pr.print("{ ")
      genJS(body)
      if (!isBlock(body)) pr.print(" }")
      pr
  }

  private def isBlock (tree :Tree) = tree.isInstanceOf[Block]
  private def genSym (sym :Symbol)(implicit pr :Printer) :Printer = pr.print(sym.name)
}
