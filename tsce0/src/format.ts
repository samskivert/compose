import * as M from "./markup"
import * as T from "./trees"
import { Name } from "./names"

export function formatDef (def :T.Def, focus :T.Path) :{elem :M.Elem, path :M.Path} {
  console.log(`formatDef ${def} @ ${focus}`)
  return new Acc(def, focus).appendDef(T.emptyPath, def).finalize()
}

export function formatExpr (expr :T.Expr, focus :T.Path) :{elem :M.Elem, path :M.Path} {
  return new Acc(expr, focus).appendExpr(T.emptyPath, expr).finalize()
}

class Acc {
  line :M.Span[] = []
  block :M.Elem[] = []
  markupPath :M.Path = M.emptyPath

  constructor (readonly root :T.Tree, readonly focus :T.Path) {}

  appendTree (path :T.Path, tree :T.Tree) :this {
    if (tree instanceof T.Def) return this.appendDef(path, tree)
    else if (tree instanceof T.Type) return this.appendType(path, tree)
    else if (tree instanceof T.Expr) return this.appendExpr(path, tree)
    else throw new Error(`Unexpected tree: ${tree}`)
  }

  appendDef (path :T.Path, def :T.Def) :this {
    if (def instanceof T.TermDef) {
      this.appendKeySpan("def ")
      this.appendAbs(path, def.name, T.typeUnknown, def.expr, {path, tpe: def.tpe})

    } else if (def instanceof T.TypeDef) {
      this.appendKeySpan("type ")
      this.appendTAbs(path, def.name, def.tpe)

    } else if (def instanceof T.DataDef) {
      this.appendKeySpan("data ")
      this.appendTAbs(path, def.name, def.tpe)
    }
    return this
  }

  appendType (path :T.Path, tpe :T.Type) :this {
    if (tpe instanceof T.Unknown) {
      // nothing
    } else if (tpe instanceof T.TypeHole) {
      this.appendTypeExprSpan(path, "?")
    } else if (tpe instanceof T.Const) {
      this.appendTypeExprSpan(path, tpe.cnst.value)
    } else if (tpe instanceof T.Data) {
      this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)
    } else if (tpe instanceof T.Arrow) {
      this.appendType(T.extendPath(path, 0), tpe.from)
      this.appendKeySpan(" → ")
      this.appendType(T.extendPath(path, 1), tpe.to)
    } else if (tpe instanceof T.TRef) {
      this.appendTypeExprSpan(path, tpe.name)
    } else if (tpe instanceof T.TAbs) {
      this.appendTAbs(path, tpe.name, tpe.body)
    } else if (tpe instanceof T.TApp) {
      this.appendType(T.extendPath(path, 0), tpe.ctor)
      this.appendSepSpan(" ")
      this.appendType(T.extendPath(path, 1), tpe.arg)
    } else if (tpe instanceof T.Sum) {
      for (let ii = 0; ii < tpe.records.length; ii += 1) {
        this.newLine()
        this.appendSubTree(T.extendPath(path, ii), tpe.records[ii])
      }
    } else if (tpe instanceof T.Record) {
      this.appendTypeDefSpan(T.extendPath(path, 0), tpe.name)
      for (let ii = 0; ii < tpe.fields.length; ii += 1) {
        // TODO: newline separate if documented?
        this.appendSepSpan(" ")
        this.appendType(T.extendPath(path, (ii+1)), tpe.fields[ii])
      }
    } else if (tpe instanceof T.Field) {
      this.appendTermDefSpan(T.extendPath(path, 0), tpe.name)
      this.appendAnnType(T.extendPath(path, 0), tpe.tpe)
    } else {
      throw new Error(`Unexpected type: ${tpe}`)
    }
    return this
  }

  appendAnnType (path :T.Path, tpe :T.Type) {
    if (!(tpe instanceof T.Unknown)) {
      this.appendKeySpan(":")
      this.appendType(path, tpe)
    } else if (T.pathsEqual(path, this.focus)) {
      this.appendKeySpan(":")
      this.appendTypeExprSpan(path, "?")
    } // otherwise append nothing
  }

  appendExpr (path :T.Path, expr :T.Expr) :this {
    if (expr instanceof T.Lit) {
      this.appendExprSpan(path, expr.cnst.value, "constant")
    } else if (expr instanceof T.Ref) {
      this.appendExprSpan(path, expr.name, "ident")
    } else if (expr instanceof T.ExprHole) {
      this.appendExprSpan(path, "?", "type")

    } else if (expr instanceof T.App) {
      this.appendExpr(T.extendPath(path, 0), expr.fun)
      this.appendSepSpan(" ")
      this.appendExpr(T.extendPath(path, 1), expr.arg)
    } else if (expr instanceof T.Let) {
      this.appendKeySpan("let ")
      this.appendAbs(path, expr.name, T.typeUnknown, expr.value, {path, tpe: expr.tpe})
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
        this.appendSubTree(T.extendPath(path, (ii+1)), expr.cases[ii])
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
  appendTypeDefSpan (path :T.Path, name :string, bodyPath? :T.Path) {
    const editor = new TypeNameEditor(this.root, path, bodyPath)
    this.appendSpan(M.span(name, editor, ["type"]), path)
  }
  appendTypeExprSpan (path :T.Path, text :string, bodyPath? :T.Path) {
    const editor = new TypeExprEditor(this.root, path, bodyPath)
    this.appendSpan(M.span(text, editor, ["type"]), path)
  }
  appendTermDefSpan (path :T.Path, name :string, typePath? :T.Path, bodyPath? :T.Path) {
    const editor = new TermNameEditor(this.root, path, typePath, bodyPath)
    this.appendSpan(M.span(name, editor, ["def"]), path)
  }
  appendExprSpan (path :T.Path, name :string, style :string) {
    const editor = new TermExprEditor(this.root, path)
    this.appendSpan(M.span(name, editor, [style]), path)
  }

  appendSubTree (path :T.Path, tree :T.Tree) {
    // propagate our target path to the formatter for the sub-expr
    let sub = new Acc(this.root, this.focus).appendTree(path, tree).finalize()
    // if the target tree element was in the sub-expr, capture the markup path
    if (sub.path != M.emptyPath) {
      // prepend the path to the block we're about to append
      this.markupPath = {idxs: [this.block.length].concat(sub.path.idxs), span: sub.path.span}
    }
    this.block.push(sub.elem)
  }

  // Coalescing abs:
  // - abs inspects body: if it's another abs, append the arg & 'lift' the body
  appendAbs (path :T.Path, name :Name, tpe :T.Type, body :T.Expr,
             arrow? :{path :T.Path, tpe :T.Type}) {
    const typePath = T.extendPath(path, 1)
    const bodyPath = T.extendPath(path, 2)
    this.appendTermDefSpan(T.extendPath(path, 0), name, typePath, bodyPath)
    this.appendAnnType(T.extendPath(path, 1), tpe)
    if (body instanceof T.Abs) {
      this.appendSepSpan(" ")
      this.appendAbs(bodyPath, body.name, body.tpe, body.body, arrow)
    } else {
      if (arrow && arrow.tpe != T.typeUnknown) {
        this.appendKeySpan(" ⇒ ")
        this.appendType(T.extendPath(arrow.path, 1), arrow.tpe)
      }
      this.appendKeySpan(" = ")
      this.newLine()
      this.appendSubTree(bodyPath, body)
    }
  }
  appendTAbs (path :T.Path, name :Name, body :T.Type) {
    const bodyPath = T.extendPath(path, 1)
    this.appendTypeDefSpan(T.extendPath(path, 0), name, bodyPath)
    if (body instanceof T.TAbs) {
      this.appendSepSpan(" ")
      this.appendTAbs(bodyPath, body.name, body.body)
    } else {
      this.appendKeySpan(" = ")
      this.appendType(bodyPath, body)
    }
  }

  // Coalescing let:
  // - let inspects body: if it's another let, append the let specially & 'lift' the body
  // TODO: appendLet (...)
}

class TypeNameEditor extends M.Editor {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly bodyPath? :T.Path
  ) { super () }

  get placeHolder () { return "<name>" }

  handleKey (text :string, key :string) :M.EditAction {
    console.log(`nameEdit ${key} @ ${text}`)
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      return this.commitEdit(text)
    case ":":
      // TODO: should we just allow : in type names? could be problematic?
      return this.commitEdit(text)
    case " ":
      return this.bodyPath ? ({
        tree: this.root.editName(_ => text, this.path)
                       .editExpr(body => new T.TAbs("", body), this.bodyPath),
        focus: this.bodyPath.concat([0])
      }) : this.commitEdit(text)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string) {
    return {tree: this.root.editName(_ => text, this.path)}
  }
}

class TypeExprEditor extends M.Editor {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly bodyPath? :T.Path
  ) { super () }

  get placeHolder () { return "<type>" }

  handleKey (text :string, key :string) :M.EditAction {
    switch (key) {
    case "Enter":
    case "Tab":
      return {tree: this.root.editType(_ => new T.TRef(text), this.path)}
    case " ":
      return this.bodyPath ?
        ({tree: this.root.editType(body => new T.TAbs(text, body), this.bodyPath)}) :
        ({tree: this.root.editType(_ => new T.TRef(text), this.path)})
    case "Escape": return "cancel"
    default: return "extend"
    }
  }
}

class TermNameEditor extends M.Editor {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly typePath? :T.Path,
    readonly bodyPath? :T.Path
  ) { super () }

  get placeHolder () { return "<name>" }

  handleKey (text :string, key :string) :M.EditAction {
    console.log(`nameEdit ${key} @ ${text}`)
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      return this.commitEdit(text)
    case ":":
      return this.typePath ?
        ({tree: this.root.editName(_ => text, this.path), focus: this.typePath}) :
        this.commitEdit(text)
    case " ":
      return this.bodyPath ? ({
        tree: this.root.editName(_ => text, this.path)
                       .editExpr(body => new T.Abs("", T.typeUnknown, body), this.bodyPath),
        focus: this.bodyPath.concat([0])
      }) : this.commitEdit(text)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string) {
    return {tree: this.root.editName(_ => text, this.path)}
  }
}

class TermExprEditor extends M.Editor {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path
  ) { super () }

  get placeHolder () { return "<expr>" }

  handleKey (text :string, key :string) :M.EditAction {
    console.log(`exprEdit ${key} @ ${text}`)
    switch (key) {
    case "Enter":
    case "Tab":
    case " ":
      return this.commitEdit(text)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  editTree (leaf :T.Tree, fsuff? :number[]) {
    const tree = this.root.editExpr(_ => leaf, this.path)
    return fsuff ? {tree, focus: this.path.concat(fsuff)} : {tree}
  }

  commitEdit (text :string) {
    switch (text) {
    case "let":
      return this.editTree(new T.Let("", T.typeUnknown, T.exprHole, T.exprHole), [0])
    case "case":
      return this.editTree(new T.Case(T.exprHole, [new T.CaseCase(T.exprHole, T.exprHole)]), [0])
    default:
      return this.editTree(new T.Ref(text))
    }
  }
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
