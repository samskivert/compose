import * as M from './markup'
import * as T from './trees'
import { Name } from './names'

export function formatDef (def :T.Def, focus :T.Path) :{elem :M.Elem, path :M.Path} {
  return new Acc(focus).appendDef(T.emptyPath, def).finalize()
}

export function formatExpr (expr :T.Expr, focus :T.Path) :{elem :M.Elem, path :M.Path} {
  return new Acc(focus).appendExpr(T.emptyPath, expr).finalize()
}

class Acc {
  focus :T.Path
  line :M.Span[] = []
  block :M.Elem[] = []
  markupPath :M.Path = M.emptyPath

  constructor (focus :T.Path) {
    this.focus = focus
  }

  appendDef (path :T.Path, def :T.Def) :this {
    if (def instanceof T.Term) {
      this.appendKeySpan("def ")
      this.appendAbs(path, def.name, T.typeUnknown, def.expr, def.tpe, path)

    } else if (def instanceof T.Union) {
      this.appendKeySpan("data ")
      this.appendDefSpan(T.extendPath(path, 0), def.name)
      this.appendKeySpan(" =")
      for (let ii = 0; ii < def.records.length; ii += 1) {
        this.newLine()
        this.appendSubExpr(T.extendPath(path, (ii+1)), def.records[ii])
      }

    } else if (def instanceof T.Record) {
      // this.appendKeySpan("data ") // TODO: only if not union case
      this.appendDefSpan(T.extendPath(path, 0), def.name)
      this.appendKeySpan(" (")
      for (let ii = 0; ii < def.fields.length; ii += 1) {
        // TODO: newline separate if documented...
        if (ii > 0) this.appendSepSpan(", ")
        this.appendExpr(T.extendPath(path, (ii+1)), def.fields[ii])
      }
      this.appendKeySpan(")")

    } else if (def instanceof T.Field) {
      this.appendDefSpan(T.extendPath(path, 0), def.name)
      this.appendType(T.extendPath(path, 0), def.tpe)
    }
    return this
  }

  appendExpr (path :T.Path, expr :T.Expr) :this {
    if (expr instanceof T.Lit) {
      this.appendExprSpan(path, expr.cnst.value, "constant")
    } else if (expr instanceof T.Ref) {
      this.appendExprSpan(path, expr.name, "ident")
    } else if (expr instanceof T.Hole) {
      this.appendExprSpan(path, "?", "type")
    } else if (expr instanceof T.App) {
      this.appendExpr(T.extendPath(path, 0), expr.fun)
      this.appendSepSpan(" ")
      this.appendExpr(T.extendPath(path, 1), expr.arg)
    } else if (expr instanceof T.Let) {
      this.appendKeySpan("let ")
      this.appendAbs(path, expr.name, T.typeUnknown, expr.value, expr.tpe, path)
      this.newLine()
      this.appendKeySpan("in ")
      this.appendExpr(T.extendPath(path, 3), expr.body)
    } else if (expr instanceof T.Abs) {
      this.appendAbs(path, expr.name, expr.tpe, expr.body)
    } else if (expr instanceof T.If) {
      this.appendKeySpan("todo if")
    } else if (expr instanceof T.Case) {
      this.appendKeySpan("case ")
      this.appendExpr(T.extendPath(path, 0), expr.scrut)
      this.appendKeySpan(" of")
      for (let ii = 0; ii < expr.cases.length; ii += 1) {
        this.newLine()
        this.appendSubExpr(T.extendPath(path, (ii+1)), expr.cases[ii])
      }
    } else if (expr instanceof T.CaseCase) {
      this.appendExpr(T.extendPath(path, 0), expr.pat)
      this.appendKeySpan(" → ")
      this.appendExpr(T.extendPath(path, 1), expr.body)
    }
    return this
  }

  finalize () :{elem :M.Elem, path :M.Path} {
    // TODO: assert that acc.line is empty?
    this.newLine()
    return {elem: new M.Block(this.block), path: this.markupPath}
  }

  /** Adds span to accumulating line. */
  appendSpan (span :M.Span, path? :T.Path) {
    // if we're appending our target tree node, capture the path to it
    if (path && T.pathsEqual(path, this.focus)) {
      this.markupPath = M.mkPath([this.block.length], this.line.length)
    }
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

  appendKeySpan (text :string) {
    this.appendSpan(M.span(text, undefined, ["keyword"]))
  }
  appendSepSpan (text :string) {
    this.appendSpan(M.span(text))
  }
  appendTypeSpan (path :T.Path, text :string) {
    const typeEditor :M.EditFn = (text, tree) => ({
      tree: tree.editType(_ => new T.TRef(text), path)})
    this.appendSpan(M.span(text, typeEditor, ["type"]), path)
  }
  appendDefSpan (path :T.Path, name :string) {
    const nameEditor :M.EditFn = (text, tree) => ({tree: tree.editName(_ => text, path)})
    this.appendSpan(M.span(name, nameEditor, ["def"]), path)
  }
  appendExprSpan (path :T.Path, name :string, style :string) {
    function editTree (root :T.Tree, path :T.Path, leaf :T.Tree, fsuff? :number[]) {
      const tree = root.editExpr(_ => leaf, path)
      return fsuff ? {tree, focus: path.concat(fsuff)} : {tree}
    }
    const exprEditor :M.EditFn = (text, tree) => {
      if (text == "let") return editTree(
        tree, path, new T.Let("", T.typeUnknown, T.exprHole, T.exprHole), [0])
      else return editTree(tree, path, new T.Ref(text), [])
    }
    this.appendSpan(M.span(name, exprEditor, [style]), path)
  }

  appendSubExpr (path :T.Path, expr :T.Expr) {
    // propagate our target path to the formatter for the sub-expr
    let sub = new Acc(this.focus).appendExpr(path, expr).finalize()
    // if the target tree element was in the sub-expr, capture the markup path
    if (sub.path != M.emptyPath) {
      // prepend the path to the block we're about to append
      this.markupPath = {idxs: [this.block.length].concat(sub.path.idxs), span: sub.path.span}
    }
    this.block.push(sub.elem)
  }

  appendType (path :T.Path, tpe :T.Type) {
    if (!(tpe instanceof T.Unknown)) {
      this.appendKeySpan(":")
      this.appendBareType(path, tpe)
    } // otherwise append nothing
  }

  appendBareType (path :T.Path, tpe :T.Type) {
    if (tpe instanceof T.Unknown) {
      // nothing
    } else if (tpe instanceof T.Const) {
      this.appendTypeSpan(path, tpe.cnst.value)
    } else if (tpe instanceof T.Data) {
      this.appendTypeSpan(path, `${tpe.tag}${tpe.size}`)
    } else if (tpe instanceof T.Arrow) {
      this.appendBareType(T.extendPath(path, 0), tpe.from)
      this.appendKeySpan(" → ")
      this.appendBareType(T.extendPath(path, 1), tpe.to)
    } else if (tpe instanceof T.TRef) {
      this.appendTypeSpan(path, tpe.name)
    } else if (tpe instanceof T.TVar) {
      this.appendTypeSpan(path, tpe.name)
    } else if (tpe instanceof T.TApp) {
      this.appendBareType(T.extendPath(path, 0), tpe.ctor)
      this.appendSepSpan(" ")
      this.appendBareType(T.extendPath(path, 1), tpe.arg)
    } else {
      throw new Error(`Unexpected type: ${tpe}`)
    }
  }

  // Coalescing abs:
  // - abs inspects body: if it's another abs, append the arg & 'lift' the body
  appendAbs (path :T.Path, name :Name, tpe :T.Type, body :T.Expr,
             arrowTpe? :T.Type, arrowPath? :T.Path) {
    if (body instanceof T.Abs) {
      this.appendDefSpan(T.extendPath(path, 0), name)
      this.appendType(T.extendPath(path, 1), tpe)
      this.appendSepSpan(" ")
      this.appendAbs(T.extendPath(path, 2), body.name, body.tpe, body.body, arrowTpe, arrowPath)
    } else {
      this.appendDefSpan(T.extendPath(path, 0), name)
      this.appendType(T.extendPath(path, 1), tpe)
      if (arrowTpe && arrowPath && arrowTpe != T.typeUnknown) {
        this.appendKeySpan(" => ")
        this.appendBareType(T.extendPath(arrowPath, 1), arrowTpe)
      }
      this.appendKeySpan(" = ")
      this.newLine()
      this.appendSubExpr(T.extendPath(path, 2), body)
    }
  }

  // Coalescing let:
  // - let inspects body: if it's another let, append the let specially & 'lift' the body
  // TODO: appendLet (...)
}

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
