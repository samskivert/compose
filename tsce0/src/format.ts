import * as M from "./markup"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"

export function format (tree :T.Tree, focus :T.Path) :{elem :M.Elem, path :M.Path} {
  console.log(`format ${tree} @ ${focus}`)
  return new Acc(tree, focus).appendTree(T.emptyPath, tree, true).finalize()
}

class Acc {
  line :M.Span[] = []
  block :M.Elem[] = []
  markupPath :M.Path = M.emptyPath

  constructor (readonly root :T.Tree, readonly focus :T.Path) {}

  appendTree (path :T.Path, tree :T.Tree, topLevel = false) :this {
    switch (tree.kind) {
    case "term":
      this.appendKeySpan("term ")
      this.appendAbs(path, tree.symAt(0), T.emptyTree, tree.treeAt(2), {path, tpe: tree.treeAt(1)})
      break

    case "type":
      this.appendKeySpan("type ")
      this.appendTAbs(path, tree.symAt(0), tree.treeAt(1), true)
      break

    case "empty":
      // nothing (TODO)
      break

    case "thole":
      this.appendTypeExprSpan(path, "?")
      break

    case "tconst":
      this.appendTypeExprSpan(path, tree.constAt(0).value)
      break

    // } else if (tpe instanceof T.ScalarDef) {
    //   this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)

    case "arrow":
      this.appendTree(T.extendPath(path, 0), tree.treeAt(0))
      this.appendKeySpan(" → ")
      this.appendTree(T.extendPath(path, 1), tree.treeAt(1))
      break

    case "tref":
      this.appendTypeExprSpan(path, tree.symAt(0).name)
      break

    case "tabs":
      this.appendTAbs(path, tree.symAt(0), tree.treeAt(1))
      break

    case "tapp":
      this.appendTree(T.extendPath(path, 0), tree.treeAt(0))
      this.appendSepSpan(" ")
      this.appendTree(T.extendPath(path, 1), tree.treeAt(1))
      break

    case "prod":
      for (let ii = 0; ii < tree.branches.length; ii += 1) {
        // TODO: newline separate if documented?
        this.appendSepSpan(" ")
        this.appendTree(T.extendPath(path, (ii+1)), tree.treeAt(ii))
      }
      break

    case "sum":
      for (let ii = 0; ii < tree.branches.length; ii += 1) {
        this.newLine()
        this.appendSubTree(T.extendPath(path, ii), tree.treeAt(ii))
      }
      break

    case "lit":
      this.appendExprSpan(path, tree.constAt(0).value, "constant")
      break

    case "ref":
      this.appendExprSpan(path, tree.symAt(0).name, "ident")
      break

    case "hole":
      this.appendExprSpan(path, "?", "type")
      break

    case "app":
    case "inapp":
      this.appendTree(T.extendPath(path, 0), tree.treeAt(0))
      this.appendSepSpan(" ")
      this.appendTree(T.extendPath(path, 1), tree.treeAt(1))
      break

    case "let":
      this.appendKeySpan("let ")
      this.appendAbs(path, tree.symAt(0), T.emptyTree, tree.treeAt(2), {path, tpe: tree.treeAt(1)})
      this.newLine()
      this.appendKeySpan("in ")
      this.appendTree(T.extendPath(path, 3), tree.treeAt(3))
      break

    case "all":
      const bodyPath = T.extendPath(path, 1)
      this.appendKeySpan("∀")
      this.appendTypeDefSpan(T.extendPath(path, 0), tree.symAt(0), bodyPath)
      this.appendSepSpan(" ")
      this.appendTree(bodyPath, tree.treeAt(1))
      break

    case "abs":
      this.appendAbs(path, tree.symAt(0), tree.treeAt(1), tree.treeAt(2))
      break

    case "if":
      this.appendKeySpan("todo if")
      break

    case "match":
      this.appendKeySpan("case ")
      this.appendTree(T.extendPath(path, 0), tree.treeAt(0))
      this.appendKeySpan(" of")
      for (let ii = 1; ii < tree.branches.length; ii += 1) {
        this.newLine()
        this.appendSubTree(T.extendPath(path, (ii+1)), tree.treeAt(ii))
      }
      break

    case "case":
      this.appendTree(T.extendPath(path, 0), tree.treeAt(0))
      this.appendKeySpan(" → ")
      this.appendTree(T.extendPath(path, 1), tree.treeAt(1))
      break

    default:
      throw new Error(`Unexpected tree: ${tree}`)
    }

    return this
  }

  appendAnnType (path :T.Path, tree :T.Tree) {
    if (tree.kind !== "thole") {
      this.appendKeySpan(":")
      this.appendTree(path, tree)
    } else if (T.pathsEqual(path, this.focus)) {
      this.appendKeySpan(":")
      this.appendTypeExprSpan(path, "?")
    } // otherwise append nothing
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
  appendTypeDefSpan (path :T.Path, sym :S.Symbol, bodyPath? :T.Path) {
    const editor = new TypeNameEditor(this.root, path, bodyPath)
    this.appendSpan(M.span(sym.name, editor, ["type"]), path)
  }
  appendTypeExprSpan (path :T.Path, text :string, bodyPath? :T.Path) {
    const editor = new TypeExprEditor(this.root, path, bodyPath)
    this.appendSpan(M.span(text, editor, ["type"]), path)
  }
  appendTermDefSpan (path :T.Path, name :string, typePath? :T.Path, bodyPath? :T.Path) {
    const editor = new TermNameEditor(this.root, path, typePath, bodyPath)
    const text = name == "" ? "?" : name
    this.appendSpan(M.span(text, editor, ["def"]), path)
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
  appendAbs (path :T.Path, sym :S.Symbol, tpe :T.Tree, body :T.Tree,
             arrow? :{path :T.Path, tpe :T.Tree}) {
    const typePath = T.extendPath(path, 1)
    const bodyPath = T.extendPath(path, 2)
    this.appendTermDefSpan(T.extendPath(path, 0), sym.name, typePath, bodyPath)
    this.appendAnnType(T.extendPath(path, 1), tpe)
    switch (body.kind) {
    case "abs":
      this.appendSepSpan(" ")
      this.appendAbs(bodyPath, body.symAt(0), body.treeAt(1), body.treeAt(2), arrow)
      break
    case "all":
      this.appendSepSpan(" ")
      this.appendAll(bodyPath, body.symAt(0), body.treeAt(1), arrow)
      break
    default:
      if (arrow && arrow.tpe !== T.emptyTree) {
        this.appendKeySpan(" ⇒ ")
        this.appendTree(T.extendPath(arrow.path, 1), arrow.tpe)
      }
      this.appendKeySpan(" = ")
      this.newLine()
      this.appendSubTree(bodyPath, body)
      break
    }
  }
  appendAll (path :T.Path, sym :S.Symbol, body :T.Tree, arrow? :{path :T.Path, tpe :T.Tree}) {
    const bodyPath = T.extendPath(path, 1)
    this.appendKeySpan("∀")
    this.appendTypeDefSpan(T.extendPath(path, 0), sym, bodyPath)
    switch (body.kind) {
    case "all":
      this.appendSepSpan(" ")
      this.appendAll(bodyPath, body.symAt(0), body.treeAt(1))
      break
    case "abs":
      this.appendSepSpan(" ")
      this.appendAbs(bodyPath, body.symAt(0), body.treeAt(1), body.treeAt(2), arrow)
      break
    default:
      // TODO: this shouldn't really ever happen, we should complain?
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, body)
      break
    }
  }
  appendTAbs (path :T.Path, sym :S.Symbol, body :T.Tree, topDef = false) {
    const bodyPath = T.extendPath(path, 1)
    if (!topDef) this.appendKeySpan("∀")
    this.appendTypeDefSpan(T.extendPath(path, 0), sym, bodyPath)
    if (body.kind == "tabs") {
      this.appendSepSpan(" ")
      this.appendTAbs(bodyPath, body.symAt(0), body.treeAt(1))
    } else {
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, body)
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
        tree: this.root.edit(this.path, te => te.setName(text))
                       .edit(this.bodyPath, te => te.setTAbs("", te.currentTree)),
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
}

class TypeExprEditor extends M.Editor {
  constructor (
    readonly root :T.Tree,
    readonly path :T.Path,
    readonly bodyPath? :T.Path
  ) { super () }

  get placeHolder () { return "<type>" }

  handleKey (text :string, key :string) :M.EditAction {
    const setRef = (te :T.TreeEditor) =>
      text == "" ? te.setTHole() : te.setTRef(te.tree.scope.lookupType(text))
    switch (key) {
    case "Enter":
    case "Tab":
      return {tree: this.root.edit(this.path, setRef)}
    case " ":
      return this.bodyPath ?
        ({tree: this.root.edit(this.bodyPath, te => te.setTAbs("", te.currentTree))}) :
        ({tree: this.root.edit(this.path, setRef)})
    case "Escape":
      return "cancel"
    default:
      return "extend"
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
    const setName = (te :T.TreeEditor) => te.setName(text)
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      return this.commitEdit(text)
    case ":":
      return this.typePath ?
        ({tree: this.root.edit(this.path, setName), focus: this.typePath}) :
        this.commitEdit(text)
    case " ":
      return this.bodyPath ? ({
        tree: this.root.edit(this.path, setName)
                       .edit(this.bodyPath, te => te.setTAbs("", te.currentTree)),
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
}
