import * as M from './markup'
import * as T from './trees'
import * as TP from './types'
import { Name } from './names'

// | Lit Constant => append span
// | + "hello"
// | + 42
// | + false
// |
// | Ref Name => append span
// | + foo
// |
// | App Expr Expr => append spans
// | + foo bar
// |
// | Let Name Type Expr Expr => append spans, merge nested lets, opt newline before in
// | + let name = exp in body
// |
// | + let name = exp
// |   in  body
// |
// | + let foo = fooexp  (nested let)
// |       bar = barexp
// |   in  body
// |
// | x let
// |     foo = fooexp
// |     bar = barexp
// |   in
// |     body
// |
// | Abs Name Type Expr => append spans
// | + x = body
// |
// | + x y = body  (nested abs)
// |
// | If Expr Expr Expr => append spans, opt newline before then & else
// | + if test then texp else fexp
// |
// | + if test
// |   then texp
// |   else fexp
// |
// | x if
// |     test
// |   then
// |     texp
// |   else
// |     fexp
// |
// | CaseCase Expr Expr => append spans, opt format cexp to nested block
// | + pat = cexp
// |
// | x pat =
// |     cexp
// |
// | Case Expr (Array Expr) => append spans, format cases to nested block
// | + case scrut of
// |     casecase
// |     casecase
// |     ...
// |
// | x case scrut of casecase casecase ...

// Coalescing let and abs:
// - let inspects body: if it's another let, append the let specially & 'lift' the body
// - abs inspects body: if it's another abs, append the arg & 'lift' the body

class Acc {
  line :M.Span[] = []
  block :M.Elem[] = []

  /** Adds span to accumulating line. */
  appendSpan (span :M.Span) {
    this.line.push(span)
  }

  /** Adds accumulating line to accumulating block, sets accumulating line to `[]`.
    * Only if current accumulating line is non-empty, otherwise it noops. */
  newLine () {
    if (this.line.length > 0) {
      this.block.push(new M.Line(this.line))
      this.line = []
    }
  }

  /** Appends nested block. */
  appendBlock (block :M.Elem) {
    this.block.push(block)
  }

  appendType (path :T.Path, tpe :TP.Type) {
    if (!(tpe instanceof TP.Unknown)) {
      this.appendSpan(M.keySpan(":"))
      this.appendBareType(path, tpe)
    } // otherwise append nothing
  }

  appendAbs (path :T.Path, name :Name, tpe :TP.Type, body :T.Expr) {
    if (body instanceof T.Abs) {
      this.appendSpan(M.defSpan(name, nameEditor(T.extendPath(path, 0))))
      this.appendType(T.extendPath(path, 1), tpe)
      this.appendSpan(M.span(" "))
      this.appendAbs(T.extendPath(path, 2), body.name, body.tpe, body.body)
    } else {
      this.appendSpan(M.defSpan(name, nameEditor(T.extendPath(path, 0))))
      this.appendType(T.extendPath(path, 1), tpe)
      this.appendSpan(M.keySpan(" = "))
      this.newLine()
      this.appendBlock(formatSubExpr(T.extendPath(path, 2), body))
    }
  }

  private appendBareType (path :T.Path, tpe :TP.Type) {
    if (tpe instanceof TP.Unknown) {}
    else if (tpe instanceof TP.Const) {
      this.appendSpan(M.typeSpan(tpe.cnst.value, todoEditor(path)))
    } else if (tpe instanceof TP.Data) {
      this.appendSpan(M.typeSpan(`${tpe.tag}${tpe.size}`, todoEditor(path)))
    } else if (tpe instanceof TP.Arrow) {
      this.appendBareType(T.extendPath(path, 0), tpe.from)
      this.appendSpan(M.keySpan(" → "))
      this.appendBareType(T.extendPath(path, 1), tpe.to)
    } else if (tpe instanceof TP.Ctor) {
      this.appendSpan(M.typeSpan(tpe.name, nameEditor(path)))
    } else if (tpe instanceof TP.Var) {
      this.appendSpan(M.typeSpan(tpe.name, nameEditor(path)))
    } else if (tpe instanceof TP.Apply) {
      this.appendBareType(T.extendPath(path, 0), tpe.ctor)
      this.appendSpan(M.span(" "))
      this.appendBareType(T.extendPath(path, 1), tpe.arg)
    } else {
      throw new Error(`Unexpected type: ${tpe}`)
    }
  }

  appendExpr (path :T.Path, expr :T.Expr) {
    if (expr instanceof T.Lit) {
      this.appendSpan(M.constantSpan(expr.cnst.value, todoEditor(path)))
    } else if (expr instanceof T.Ref) {
      this.appendSpan(M.identSpan(expr.name, nameEditor(T.extendPath(path, 0))))
    } else if (expr instanceof T.Hole) {
      this.appendSpan(M.holeSpan(todoEditor(T.extendPath(path, 0))))
    } else if (expr instanceof T.App) {
      this.appendExpr(T.extendPath(path, 0), expr.fun)
      this.appendSpan(M.span(" "))
      this.appendExpr(T.extendPath(path, 1), expr.arg)
    } else if (expr instanceof T.Let) {
      this.appendSpan(M.keySpan("let "))
      this.appendAbs(path, expr.name, expr.tpe, expr.value)
      this.newLine()
      this.appendSpan(M.keySpan("in "))
      this.appendExpr(T.extendPath(path, 3), expr.body)
    } else if (expr instanceof T.Abs) {
      this.appendAbs(path, expr.name, expr.tpe, expr.body)
    } else if (expr instanceof T.If) {
      this.appendSpan(M.keySpan("todo if"))
    } else if (expr instanceof T.Case) {
      this.appendSpan(M.keySpan("case "))
      this.appendExpr(T.extendPath(path, 0), expr.scrut)
      this.appendSpan(M.keySpan(" of"))
      for (let ii = 0; ii < expr.cases.length; ii += 1) {
        this.newLine()
        this.appendBlock(formatSubExpr(T.extendPath(path, (ii+1)), expr.cases[ii]))
      }
    } else if (expr instanceof T.CaseCase) {
      this.appendExpr(T.extendPath(path, 0), expr.pat)
      this.appendSpan(M.keySpan(" → "))
      this.appendExpr(T.extendPath(path, 1), expr.body)
    }
  }

  appendDef (path :T.Path, def :T.Def) {
    if (def instanceof T.Term) {
      // appendSpan $ M.keySpan "def "
      this.appendAbs(path, def.name, TP.typeUnknown, def.expr)

    } else if (def instanceof T.Union) {
      this.appendSpan(M.keySpan("data "))
      this.appendSpan(M.identSpan(def.name, nameEditor(T.extendPath(path, 0))))
      this.appendSpan(M.keySpan(" ="))
      for (let ii = 0; ii < def.records.length; ii += 1) {
        this.newLine()
        this.appendBlock(formatSubExpr(T.extendPath(path, (ii+1)), def.records[ii]))
      }

    } else if (def instanceof T.Record) {
      // this.appendSpan(M.keySpan("data ")) // TODO: only if not union case
      this.appendSpan(M.identSpan(def.name, nameEditor(T.extendPath(path, 0))))
      this.appendSpan(M.keySpan(" ("))
      for (let ii = 0; ii < def.fields.length; ii += 1) {
        // TODO: newline separate if documented...
        if (ii > 0) this.appendSpan(M.span(", "))
        this.appendBlock(formatSubExpr(T.extendPath(path, (ii+1)), def.fields[ii]))
      }
      this.appendSpan(M.keySpan(")"))

    } else if (def instanceof T.Field) {
      this.appendSpan(M.identSpan(def.name, nameEditor(T.extendPath(path, 0))))
      this.appendType(T.extendPath(path, 0), def.tpe)
    }
  }
}

function nameEditor (path :T.Path) :M.EditFn {
  return text => tree => tree.editName(_ => text, path)
}

function todoEditor (path :T.Path) { return undefined }

// - format into nested block:
//   - create new block (starts with blank acc line)
//   - format subexpr to nested block
//   - finalize nested block (append acc line if non-empty)
//   - do 'new line' operation
//   - append new block to acc block
function formatSubExpr (path :T.Path, expr :T.Expr) :M.Elem {
  return format(acc => acc.appendExpr(path, expr))
}

// format :: State Acc Unit -> Elem
function format (ff :(acc :Acc) => any) :M.Elem {
  const acc = new Acc()
  ff(acc)
  // TODO: assert that acc.line is empty?
  acc.newLine()
  return new M.Block(acc.block)
}

export function formatDef (def :T.Def) :M.Elem {
  return format(acc => acc.appendDef(T.emptyPath, def))
}

export function formatExpr (expr :T.Expr) :M.Elem {
  return format(acc => acc.appendExpr(T.emptyPath, expr))
}
