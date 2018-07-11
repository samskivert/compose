import * as M from "./markup"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"

export function format (tree :T.Tree, focus :T.Path) :{elem :M.Elem, path :M.Path} {
  // console.log(`format ${tree} @ ${focus}`)
  return new Acc(tree, focus).appendTree(T.emptyPath, tree, true).finalize()
}

class Acc {
  line :M.Span[] = []
  block :M.Elem[] = []
  markupPath :M.Path = M.emptyPath

  constructor (readonly root :T.Tree, readonly focus :T.Path) {}

  appendTree (path :T.Path, tree :T.Tree, topLevel = false) :this {
    const path0 = T.extendPath(path, 0)
    const path1 = T.extendPath(path, 1)
    switch (tree.kind) {
    case "fun":
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(path0, tree.scope, tree.symAt(0), undefined, path1)
      this.appendSepSpan(" ")
      this.appendTree(path1, tree.treeAt(1))
      break

    case "type": {
      if (topLevel) this.appendKeySpan("type ")
      const body = tree.treeAt(1)
      const bodyPath = path1
      this.appendTypeDefSpan(path0, tree.scope, tree.symAt(0), bodyPath)
      if (tree.treeAt(1).kind == "tabs") {
        this.appendTAbs(bodyPath, body, 0)
      } else {
        this.appendKeySpan(" = ")
        this.appendTree(bodyPath, body)
      }
      break
    }

    case "ctor":
      this.appendTypeDefSpan(path0, tree.scope, tree.symAt(0), path1)
      this.appendTree(path1, tree.treeAt(1))
      break

    case "empty":
      // nothing (TODO)
      break

    case "thole":
      this.appendTypeExprSpan(path, tree.scope, "?")
      break

    case "tconst":
      this.appendTypeExprSpan(path, tree.scope, tree.constAt(0).value)
      break

    // } else if (tpe instanceof T.ScalarDef) {
    //   this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)

    case "arrow":
      this.appendTree(path0, tree.treeAt(0))
      this.appendKeySpan(" → ")
      this.appendTree(path1, tree.treeAt(1))
      break

    case "tref":
      this.appendTypeExprSpan(path, tree.scope, tree.symAt(0).name)
      break

    case "tabs":
      this.appendTAbs(path, tree, 0)
      break

    case "tapp":
      this.appendTree(path0, tree.treeAt(0))
      // this.appendSepSpan("[")
      this.appendSepSpan(" ")
      this.appendTree(path1, tree.treeAt(1))
      // this.appendSepSpan("]")
      break

    case "field":
      this.appendTermDefSpan(path0, tree.scope, tree.symAt(0))
      this.appendAnnType(path1, tree.treeAt(1))
      break

    case "prod":
      for (let ii = 0; ii < tree.branches.length; ii += 1) {
        // TODO: newline separate if documented?
        // this.appendSepSpan(ii == 0 ? "(" : ",")
        this.appendSepSpan(" ")
        this.appendTree(T.extendPath(path, ii), tree.treeAt(ii))
      }
      // if (tree.branches.length > 0) this.appendSepSpan(")")
      break

    case "sum":
      for (let ii = 0; ii < tree.branches.length; ii += 1) {
        this.newLine()
        this.appendSubTree(T.extendPath(path, ii), tree.treeAt(ii))
      }
      break

    case "lit":
      this.appendExprSpan(path, tree.scope, tree.constAt(0).value, "constant")
      break

    case "ref":
      const refsym = tree.symAt(0)
      this.appendExprSpan(path, tree.scope, refsym.name, refsym.kind)
      break

    case "hole":
      this.appendExprSpan(path, tree.scope, "?", "term")
      break

    case "phole":
    case "plit":
    case "pbind":
    case "pdtor":
      this.appendPattern(path, tree, 0)
      break

    case "app":
      this.appendApp(path, tree)
      break

    case "inapp":
      this.appendTree(path0, tree.treeAt(0))
      this.appendSepSpan(" ")
      this.appendTree(path1, tree.treeAt(1))
      break

    case "let": {
      this.appendKeySpan("let ")
      const typePath = path1
      const bodyPath = T.extendPath(path, 2)
      this.appendTermDefSpan(path0, tree.scope, tree.symAt(0), typePath, bodyPath)
      this.appendAnnType(typePath, tree.treeAt(1))
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, tree.treeAt(2))
      this.newLine()
      this.appendKeySpan("in ")
      this.appendTree(T.extendPath(path, 3), tree.treeAt(3))
      break
    }

    case "letfun":
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(path0, tree.scope, tree.symAt(0), undefined, path1)
      this.appendSepSpan(" ")
      this.appendTree(path1, tree.treeAt(1))
      this.newLine()
      this.appendTree(T.extendPath(path, 2), tree.treeAt(2))
      break

    case "all":
      this.appendAll(path, tree, 0)
      break

    case "asc":
      this.appendTree(path0, tree.treeAt(0))
      this.appendSepSpan(":")
      this.appendTree(path1, tree.treeAt(1))
      break

    case "abs":
      this.appendAbs(path, tree, 0)
      break

    case "if":
      this.appendKeySpan("todo if")
      break

    case "match":
      this.appendKeySpan("case ")
      this.appendTree(path0, tree.treeAt(0))
      this.appendKeySpan(" of")
      for (let ii = 1; ii < tree.branches.length; ii += 1) {
        this.newLine()
        this.appendSubTree(T.extendPath(path, (ii+1)), tree.treeAt(ii))
      }
      break

    case "case":
      this.appendTree(path0, tree.treeAt(0))
      this.appendKeySpan(" → ")
      this.appendTree(path1, tree.treeAt(1))
      break

    default:
      throw new Error(`Unexpected tree: ${tree}`)
    }

    return this
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

  appendAnnType (path :T.Path, tree :T.Tree) {
    if (tree.kind !== "empty") {
      // this.appendKeySpan(":")
      this.appendSepSpan(":")
      this.appendTree(path, tree)
    } else if (T.pathsEqual(path, this.focus)) {
      this.appendSepSpan(":")
      this.appendTypeExprSpan(path, tree.scope, "?")
    } // otherwise append nothing
  }

  appendPattern (path :T.Path, tree :T.Tree, pos :number) {
    if (tree.kind === "phole" || tree.kind === "plit" ||
        tree.kind === "pbind" || tree.kind === "pdtor") {
      const bodyPath = T.extendPath(path, 1)
      // if (pos == 1) this.appendSepSpan("(")
      // else if (pos > 1) this.appendSepSpan(",")
      switch (tree.kind) {
        case "phole":
          // TODO: special editor for pattern hole
          this.appendExprSpan(path, tree.scope, "?", "term")
          break
        case "plit":
          this.appendExprSpan(path, tree.scope, tree.constAt(0).value, "constant")
          break
        case "pbind":
          this.appendTermDefSpan(path, tree.scope, tree.symAt(0), undefined, bodyPath)
          break
        case "pdtor":
          this.appendExprSpan(path, tree.scope, tree.symAt(0).name, tree.symAt(0).kind)
          break
      }
      this.appendSepSpan(" ")
      this.appendPattern(bodyPath, tree.treeAt(1), pos+1)

    } else {
      // if (pos > 1) this.appendSepSpan(")")
      this.appendKeySpan(" → ")
      this.appendTree(path, tree)
    }
  }

  appendApp (path :T.Path, tree :T.Tree) {
    const fun = tree.treeAt(0), arg = tree.treeAt(1)
    if (fun.kind === "app") {
      this.appendApp(T.extendPath(path, 0), fun)
      // this.appendSepSpan(", ")
    } else {
      this.appendTree(T.extendPath(path, 0), fun)
      // this.appendSepSpan(fun.kind === "inapp" ? " " : "(")
    }
    this.appendSepSpan(" ")
    if (fun.kind !== "inapp" && arg.kind === "app") {
      this.appendSepSpan("(")
      this.appendApp(T.extendPath(path, 1), arg)
      this.appendSepSpan(")")
    } else {
      this.appendTree(T.extendPath(path, 1), arg)
    }
    // if (fun.kind !== "inapp" && pos === 0) this.appendSepSpan(")")
  }

  // Coalescing abs:
  // - abs inspects body: if it's another abs, append the arg & 'lift' the body
  appendAbs (path :T.Path, tree :T.Tree, pos :number) {
    const body = tree.treeAt(2)
    const bodyPath = T.extendPath(path, 2)
    const typePath = T.extendPath(path, 1)
    // this.appendSepSpan(pos == 0 ? "(" : ", ")
    this.appendTermDefSpan(T.extendPath(path, 0), tree.scope, tree.symAt(0), typePath, bodyPath)
    this.appendAnnType(T.extendPath(path, 1), tree.treeAt(1))
    switch (body.kind) {
    case "abs":
      // this.appendSepSpan(" ")
      this.appendKeySpan(" → ")
      this.appendAbs(bodyPath, body, pos+1)
      break
    case "asc":
      // this.appendSepSpan(")")
      // this.appendSepSpan(":")
      this.appendKeySpan(" → ")
      this.appendTree(T.extendPath(bodyPath, 0), body.treeAt(0))
      this.appendKeySpan(" = ")
      this.newLine()
      this.appendSubTree(T.extendPath(bodyPath, 1), body.treeAt(1))
      break
    default:
      this.appendKeySpan(" = ")
      this.appendSubTree(bodyPath, body)
      break
    }
  }
  appendAll (path :T.Path, tree :T.Tree, pos :number) {
    const sym = tree.symAt(0), body = tree.treeAt(1)
    const bodyPath = T.extendPath(path, 1)
    // this.appendSepSpan(pos == 0 ? "[" : ",")
    this.appendKeySpan("∀")
    this.appendTypeDefSpan(T.extendPath(path, 0), tree.scope, sym, bodyPath)
    switch (body.kind) {
    case "all":
      this.appendAll(bodyPath, body, pos+1)
      break
    case "abs":
      // this.appendSepSpan("]")
      this.appendSepSpan(" ")
      this.appendAbs(bodyPath, body, 0)
      break
    default:
      // TODO: this shouldn't really ever happen, we should complain?
      // this.appendSepSpan("]")
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, body)
      break
    }
  }

  appendTAbs (path :T.Path, tree :T.Tree, pos :number) {
    const sym = tree.symAt(0), body = tree.treeAt(1)
    const bodyPath = T.extendPath(path, 1)
    // this.appendSepSpan(pos == 0 ? "[" : ",")
    this.appendKeySpan(" ∀")
    this.appendTypeDefSpan(T.extendPath(path, 0), tree.scope, sym, bodyPath)
    if (body.kind == "tabs") {
      this.appendSepSpan(" ")
      this.appendTAbs(bodyPath, body, pos+1)
    } else if (body.kind == "prod") {
      // this.appendSepSpan("]")
      this.appendTree(bodyPath, body)
    } else {
      // this.appendSepSpan("]")
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, body)
    }
  }

  /** Adds span to accumulating line. */
  appendSpan (span :M.Span, path? :T.Path) {
    // if we're appending our target tree node, capture the path to it
    if (path && T.pathsEqual(path, this.focus)) {
      this.markupPath = M.mkPath([this.block.length], this.line.length)
    }
    this.line.push(span)
  }

  appendKeySpan (text :string) {
    this.appendSpan(new M.TextSpan(text, ["keyword"]))
  }
  appendSepSpan (text :string) {
    this.appendSpan(new M.TextSpan(text, ["separator"]))
  }
  appendTypeDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol, bodyPath? :T.Path) {
    this.appendSpan(new TypeNameSpan(this.root, path, scope, sym, bodyPath))
  }
  appendTypeExprSpan (path :T.Path, scope :S.Scope, text :string, bodyPath? :T.Path) {
    this.appendSpan(new TypeExprSpan(this.root, path, scope, text, bodyPath))
  }
  appendTermDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol, typePath? :T.Path, bodyPath? :T.Path) {
    this.appendSpan(new TermNameSpan(this.root, path, scope, sym, typePath, bodyPath))
  }
  appendExprSpan (path :T.Path, scope :S.Scope, name :string, style :string) {
    this.appendSpan(new TermExprSpan(this.root, path, scope, name, [style]))
  }

  finalize () :{elem :M.Elem, path :M.Path} {
    // TODO: assert that acc.line is empty?
    this.newLine()
    return {elem: new M.Block(this.block), path: this.markupPath}
  }

  /** Adds accumulating line to accumulating block, sets accumulating line to `[]`.
    * Only if current accumulating line is non-empty, otherwise it noops. */
  newLine () {
    if (this.line.length > 0) {
      this.block.push(new M.Line(this.line))
      this.line = []
    }
  }

  // Coalescing let:
  // - let inspects body: if it's another let, append the let specially & 'lift' the body
  // TODO: appendLet (...)
}

class TypeNameSpan extends M.EditableSpan {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly scope :S.Scope,
    readonly sym :S.Symbol,
    readonly bodyPath? :T.Path
  ) { super () }

  get displayText () { return this.sym.name }
  get styles () { return ["type"] }
  get editPlaceHolder () { return "<name>" }

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
        tree: this.root.edit(this.path, te => te.setName(text))
                       .edit(this.bodyPath, te => te.setTAbs("").migrateChild(1, te.currentTree)),
        focus: this.bodyPath.concat([0])
      }) : this.commitEdit(text)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string) {
    return {tree: this.root.edit(this.path, te => te.setName(text))}
  }

  getCompletions (text :string) :string[] {
    return [] // TODO
  }
}

class TypeExprSpan extends M.EditableSpan {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly scope :S.Scope,
    readonly displayText :string,
    readonly bodyPath? :T.Path
  ) { super () }

  get styles () { return ["type"] }
  get editPlaceHolder () { return "<type>" }

  handleKey (text :string, key :string) :M.EditAction {
    const setRef = (te :T.TreeEditor) =>
      text == "" ? te.setTHole() : te.setTRef(te.tree.scope.lookupType(text))
    switch (key) {
    case "Enter":
    case "Tab":
      return {tree: this.root.edit(this.path, setRef)}
    case " ":
      return this.bodyPath ?
        ({tree: this.root.edit(this.bodyPath, te => te.setTAbs("").migrateChild(1, te.currentTree))}) :
        ({tree: this.root.edit(this.path, setRef)})
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  getCompletions (text :string) :string[] {
    return this.scope.getCompletions(sym => sym.kind === "type", text).map(sym => sym.name)
  }
}

class TermNameSpan extends M.EditableSpan {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly scope :S.Scope,
    readonly sym :S.Symbol,
    readonly typePath? :T.Path,
    readonly bodyPath? :T.Path
  ) { super () }

  get displayText () { return this.sym.name || "?" }
  get styles () { return [this.sym.kind] }
  get editText () { return this.sym.name }
  get editPlaceHolder () { return "<name>" }

  handleKey (text :string, key :string) :M.EditAction {
    const {root, path, bodyPath, typePath} = this
    console.log(`nameEdit ${key} @ ${text}`)
    const setName = (te :T.TreeEditor) => te.setName(text)
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      return this.commitEdit(text)
    case ":":
      if (!typePath) return this.commitEdit(text)
      return ({tree: root.edit(path, setName), focus: typePath})
    case " ":
      if (!bodyPath) return this.commitEdit(text)
      const tree = root.edit(path, setName)
                       .edit(bodyPath, te => te.setTAbs("").migrateChild(1, te.currentTree))
      return ({tree, focus: bodyPath.concat([0])})
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string) {
    return {tree: this.root.edit(this.path, te => te.setName(text))}
  }

  getCompletions (text :string) :string[] {
    return [] // TODO
  }
}

class TermExprSpan extends M.EditableSpan {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly scope :S.Scope,
    readonly displayText :string,
    readonly styles :string[]
  ) { super () }

  get editPlaceHolder () { return "<expr>" }

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

  editTree (edit :(te :T.TreeEditor) => void, fsuff? :number[]) {
    const tree = this.root.edit(this.path, edit)
    return fsuff ? {tree, focus: this.path.concat(fsuff)} : {tree}
  }

  commitEdit (text :string) {
    switch (text) {
    case "let":
      return this.editTree(te => te.setLetH(), [0])
    case "case":
      return this.editTree(te => te.setMatchH(), [0])
    case "":
      return this.editTree(te => te.setHole(TP.hole))
    default:
      return this.editTree(te => te.setRef(te.tree.scope.lookupTerm(text)))
    }
  }

  getCompletions (text :string) :string[] {
    const pred = (sym :S.Symbol) => sym.kind === "term" || sym.kind === "func"
    return this.scope.getCompletions(pred, text).map(sym => sym.name)
  }
}
