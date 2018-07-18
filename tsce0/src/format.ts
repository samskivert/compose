import * as M from "./markup"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"
import * as C from "./constants"

export function format (
  tree :T.DefTree, focus :T.Path, showSigs :boolean = true
) :{elem :M.Elem, path :M.Path} {
  // console.log(`format ${tree} @ ${focus}`)
  return new Acc(tree, focus, showSigs).appendTree(T.emptyPath, tree).finalize()
}

class SigAnnot {
  constructor (
    readonly start :number,
    readonly length :number,
    readonly depth :number,
    readonly sig :string
  ) {}
}

class Acc {
  line :M.Span[] = []
  lineWidth = 0
  sigAnnots :SigAnnot[] = []
  block :M.Elem[] = []
  markupPath :M.Path = M.emptyPath
  depth = -1

  constructor (readonly root :T.DefTree, readonly focus :T.Path, readonly showSigs :boolean) {}

  appendTree (path :T.Path, tree :T.Tree) :this {
    this.depth += 1
    const path0 = T.extendPath(path, 0)
    const path1 = T.extendPath(path, 1)
    const start = this.lineWidth
    switch (tree.kind) {
    case "fun":
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(path0, tree.scope, tree.symAt(0), undefined, path1)
      this.appendSepSpan(" ")
      this.appendTree(path1, tree.treeAt(1))
      break

    case "type": {
      if (this.depth == 0) this.appendKeySpan("type ")
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
      this.depth += 1
      this.appendTypeDefSpan(path0, tree.scope, tree.symAt(0), path1)
      this.depth -= 1
      this.appendTree(path1, tree.treeAt(1))
      this.appendTypeAnnot(start, tree.sig)
      break

    case "empty":
      // nothing (TODO)
      break

    case "thole":
      this.appendTypeExprSpan(path, tree.scope, "", tree.sig)
      break

    case "tconst":
      this.appendTypeExprSpan(path, tree.scope, tree.constAt(0).value, tree.sig)
      break

    // } else if (tpe instanceof T.ScalarDef) {
    //   this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)

    case "arrow":
      this.appendTree(path0, tree.treeAt(0))
      this.appendKeySpan(" → ")
      this.appendTree(path1, tree.treeAt(1))
      break

    case "tref":
      this.appendTypeExprSpan(path, tree.scope, tree.symAt(0).name, tree.sig)
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
      this.appendKindAnnot(start, tree.sig.kind)
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
      this.appendExprSpan(path, tree.scope, tree.constAt(0).value, tree.sig, "constant")
      break

    case "ref":
      const refsym = tree.symAt(0)
      this.appendExprSpan(path, tree.scope, refsym.name, tree.sig, refsym.kind)
      break

    case "hole":
      const holePath = T.extendPath(path, 0)
      if (this.depth == 0) this.appendSpan(new DefHoleSpan(this.root, path, tree.scope), holePath)
      else this.appendExprSpan(path, tree.scope, "", tree.sig, "term")
      break

    case "phole":
    case "plit":
    case "pbind":
    case "pdtor":
      this.appendPattern(path, tree, 0)
      break

    case "app":
      this.appendApp(path, tree)
      this.appendTypeAnnot(start, tree.sig)
      break

    case "inapp":
      this.appendTree(path0, tree.treeAt(0))
      this.appendSepSpan(" ")
      this.appendTree(path1, tree.treeAt(1))
      this.appendTypeAnnot(start, tree.sig)
      break

    case "let": {
      this.appendKeySpan("let ")
      const typePath = path1
      const bodyPath = T.extendPath(path, 2)
      this.appendTermDefSpan(path0, tree.scope, tree.symAt(0), typePath, bodyPath)
      this.appendAnnType(typePath, tree.treeAt(1))
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, tree.treeAt(2))
      this.appendTypeAnnot(start, tree.sig)
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
      this.appendTypeAnnot(start, tree.sig)
      this.newLine()
      this.appendTree(T.extendPath(path, 2), tree.treeAt(2))
      break

    case "all":
      this.appendAll(path, tree, 0)
      break
    case "abs":
      this.appendAbs(path, tree, 0)
      break

    case "asc":
      this.appendTree(path0, tree.treeAt(0))
      this.appendSepSpan(":")
      this.appendTree(path1, tree.treeAt(1))
      break

    case "if":
      this.appendKeySpan("if ")
      this.appendTree(T.extendPath(path, 0), tree.treeAt(0))
      this.appendKeySpan(" then ")
      this.appendTree(T.extendPath(path, 1), tree.treeAt(1))
      this.appendKeySpan(" else ")
      this.appendTree(T.extendPath(path, 2), tree.treeAt(2))
      this.appendTypeAnnot(start, tree.sig)
      break

    case "match":
      this.appendKeySpan("case ")
      this.appendTree(path0, tree.treeAt(0))
      this.appendKeySpan(" of")
      this.appendTypeAnnot(start, tree.sig)
      for (let ii = 1; ii < tree.branches.length; ii += 1) {
        this.newLine()
        this.appendSubTree(T.extendPath(path, ii), tree.treeAt(ii))
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

    this.depth -= 1
    return this
  }

  appendSubTree (path :T.Path, tree :T.Tree) {
    // propagate our target path to the formatter for the sub-expr
    if (this.line.length > 0) {
      this.newLine()
    }
    let sub = new Acc(this.root, this.focus, this.showSigs).appendTree(path, tree).finalize()
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
      this.appendTypeExprSpan(path, tree.scope, "?", tree.sig)
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
          this.appendExprSpan(path, tree.scope, "?", tree.sig, "term")
          break
        case "plit":
          this.appendExprSpan(path, tree.scope, tree.constAt(0).value, tree.sig, "constant")
          break
        case "pbind":
          this.appendTermDefSpan(path, tree.scope, tree.symAt(0), undefined, bodyPath)
          break
        case "pdtor":
          this.appendExprSpan(path, tree.scope, tree.symAt(0).name, tree.sig, tree.symAt(0).kind)
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
    // if (fun.kind === "app") {
    //   this.appendApp(T.extendPath(path, 0), fun)
    //   this.appendSepSpan(", ")
    // } else {
    //   this.appendTree(T.extendPath(path, 0), fun)
    //   this.appendSepSpan(fun.kind === "inapp" ? " " : "(")
    // }
    this.appendTree(T.extendPath(path, 0), fun)
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
    const argPath  = T.extendPath(path, 0)
    const typePath = T.extendPath(path, 1)
    const bodyPath = T.extendPath(path, 2)
    const body = tree.treeAt(2)
    // this.appendSepSpan(pos == 0 ? "(" : ", ")
    this.appendTermDefSpan(argPath, tree.scope, tree.symAt(0), typePath, bodyPath)
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
    this.lineWidth += span.displayText.length
  }

  appendKeySpan (text :string) {
    this.appendSpan(new M.TextSpan(text, ["keyword"]))
  }
  appendSepSpan (text :string) {
    this.appendSpan(new M.TextSpan(text, ["separator"]))
  }
  appendTypeDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol, bodyPath? :T.Path) {
    let start = this.lineWidth
    this.appendSpan(new TypeDefSpan(this.root, path, scope, sym, bodyPath), path)
    this.appendKindAnnot(start, sym.type.kind)
  }
  appendTypeExprSpan (path :T.Path, scope :S.Scope, text :string, type :TP.Type,
                      bodyPath? :T.Path) {
    let start = this.lineWidth
    this.appendSpan(new TypeExprSpan(this.root, path, scope, text, bodyPath), path)
    this.appendKindAnnot(start, type.kind)
  }
  appendTermDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol,
                     typePath? :T.Path, bodyPath? :T.Path) {
    let start = this.lineWidth
    this.appendSpan(new TermDefSpan(this.root, path, scope, sym, typePath, bodyPath), path)
    this.appendTypeAnnot(start, sym.type)
  }
  appendExprSpan (path :T.Path, scope :S.Scope, name :string, type :TP.Type, style :string) {
    let start = this.lineWidth
    this.appendSpan(new TermExprSpan(this.root, path, scope, name, [style]), path)
    this.appendTypeAnnot(start, type)
  }

  appendTypeAnnot (start :number, type :TP.Type) {
    if (!this.showSigs) return
    let length = this.lineWidth - start
    // console.log(`ATA ${start}/${length} @ ${this.depth} = ${type}`)
    this.sigAnnots.push(new SigAnnot(start, length, this.depth, type.toString()))
  }

  appendKindAnnot (start :number, kind :TP.Kind) {
    if (!this.showSigs) return
    let length = this.lineWidth - start
    // console.log(`AKA ${start}/${length} @ ${this.depth} = ${kind}`)
    this.sigAnnots.push(new SigAnnot(start, length, this.depth, kind.toString()))
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
      // if we have type annotations, convert them to line annotations
      const annots :M.Annot[][] = []
      const maxDepth = this.sigAnnots.map(a => a.depth).reduce((m, d) => Math.max(m, d))
      for (let dd = maxDepth ; dd >= 0 ; dd -= 1) {
        const row :M.Annot[] = []
        let pos = 0
        const depthAnnots = this.sigAnnots.filter(a => a.depth == dd)
        for (let ii = 0; ii < depthAnnots.length; ii += 1) {
          const tann = depthAnnots[ii]
          if (tann.start > pos) row.push(new M.Annot(blank(tann.start-pos), "", []))
          // TODO: better type formatting, styles
          const nann = depthAnnots[ii+1]
          const sigStr = tann.sig || "??"
          // TODO: max out avail at screen width?
          const avail = nann ? (nann.start-tann.start-1) : sigStr.length
          let annStr = sigStr
          if (sigStr.length > avail) annStr = sigStr.substring(0, avail-1)+"…"
          else if (sigStr.length < tann.length) annStr += blank(tann.length-sigStr.length)
          row.push(new M.Annot(annStr, sigStr, [`ann${dd}`]))
          pos = tann.start+annStr.length
        }
        if (pos < this.lineWidth) row.push(new M.Annot(blank(this.lineWidth-pos), "", []))
        annots.push(row)
      }
      this.block.push(new M.Line(this.line, annots))
      this.line = []
      this.lineWidth = 0
    }
  }

  // Coalescing let:
  // - let inspects body: if it's another let, append the let specially & 'lift' the body
  // TODO: appendLet (...)
}

const blank = (count :number) :string => '\u00A0'.repeat(count)

export class SymbolCompletion implements M.Completion {
  constructor (readonly item :S.Symbol) {}

  display () :M.Line {
    return new M.Line([new M.TextSpan(this.item.name)], []) // TODO
  }
}

abstract class TreeSpan extends M.EditableSpan {
  constructor(
    readonly root :T.DefTree,
    readonly path :T.Path,
    readonly scope :S.Scope) { super() }

  toString () { return `${this.displayText} @ ${this.root.pathKind(this.path)}` }
}

function mkFun (msym :S.ModuleSym) {
  return T.mkFunDef(msym, "").editBranch1(
    body => body.setAbs("").editBranches(
      undefined,
      undefined,
      body => body.setHole(TP.hole)))
}

class DefHoleSpan extends TreeSpan {
  get sourceText () { return "" }
  get styles () { return ["keyword"] }
  get displayPlaceHolder () { return "<new def>" }
  get editPlaceHolder () { return "<kind>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    console.log(`defHoleEdit ${key} @ ${text}`)
    switch (key) {
    case "Enter":
    case "Tab":
    case " ":
      return this.commitEdit(text, comp)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string, comp :M.Completion|void) :M.EditAction {
    const msym = this.root.owner as S.ModuleSym
    switch (text) {
    case "fun":
      return {tree: mkFun(msym), focus: [0]}
    }
    return "extend" // {tree: this.root}
  }
}

class TypeDefSpan extends TreeSpan {
  constructor (
    root :T.DefTree,
    path :T.Path,
    scope :S.Scope,
    readonly sym :S.Symbol,
    readonly bodyPath? :T.Path
  ) { super(root, path, scope) }

  get sourceText () { return this.sym.name }
  get styles () { return ["type"] }
  get editPlaceHolder () { return "<name>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    const {root, path, bodyPath} = this
    console.log(`typeNameEdit ${key} @ ${text}`)
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      return this.commitEdit(text, bodyPath)
    case ":":
      // TODO: should we just allow : in type names? could be problematic?
      return this.commitEdit(text)
    case " ":
      return bodyPath ? ({
        tree: root.edit(path, te => te.setName(text))
                  .edit(bodyPath, te => te.setTAbs("").adopt(1, te.currentTree)),
        focus: bodyPath.concat([0])
      }) : this.commitEdit(text)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string, focus? :T.Path) :M.EditAction {
    return {tree: this.root.edit(this.path, te => te.setName(text)), focus}
  }

  getCompletions (text :string) :M.Completion[] {
    return [] // TODO
  }
}

class TypeExprSpan extends TreeSpan {
  constructor (
    root :T.DefTree,
    path :T.Path,
    scope :S.Scope,
    readonly sourceText :string,
    readonly bodyPath? :T.Path
  ) { super(root, path, scope) }

  get styles () { return ["type"] }
  get editPlaceHolder () { return "<type>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    const setRef = (te :T.TreeEditor) =>
      text == "" ? te.setTHole() : te.setTRef(te.tree.scope.lookupType(text))
    switch (key) {
    case "Enter":
    case "Tab":
      return {tree: this.root.edit(this.path, setRef)}
    case " ":
      return this.bodyPath ?
        ({tree: this.root.edit(this.bodyPath, te => te.setTAbs("").adopt(1, te.currentTree))}) :
        ({tree: this.root.edit(this.path, setRef)})
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  getCompletions (text :string) :M.Completion[] {
    return this.scope.getCompletions(sym => sym.kind === "type", text).
      map(sym => new SymbolCompletion(sym))
  }
}

class TermDefSpan extends TreeSpan {
  constructor (
    root :T.DefTree,
    path :T.Path,
    scope :S.Scope,
    readonly sym :S.Symbol,
    readonly typePath? :T.Path,
    readonly bodyPath? :T.Path
  ) { super(root, path, scope) }

  get styles () { return [this.sym.kind] }
  get sourceText () { return this.sym.name }
  get editPlaceHolder () { return "<name>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    const {root, path, bodyPath, typePath} = this
    console.log(`termNameEdit ${key} @ ${text}`)
    const setName = (te :T.TreeEditor) => te.setName(text)
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      return this.commitEdit(text, bodyPath)
    case ":":
      if (!typePath) return this.commitEdit(text)
      return ({tree: root.edit(path, setName), focus: typePath})
    case " ":
      if (!bodyPath) return this.commitEdit(text)
      let tree = root.edit(path, setName)
      // if the body is an empty abs, we want to simply start editing it
      // if not, we want to insert an empty abs first (and then edit that)
      let child = root.select(bodyPath)
      if (!(child instanceof T.Tree) || child.kind != "abs" || child.symAt(0).name !== "") {
        tree = root.edit(bodyPath, te => {
          const oldBody = te.currentTree
          return (mods.shift ? te.setTAbs("").adopt(1, oldBody) :
                  te.setAbs("").adopt(2, oldBody))
        })
      }
      return ({tree, focus: bodyPath.concat([0])})
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string, focus? :T.Path) :M.EditAction {
    return {tree: this.root.edit(this.path, te => te.setName(text)), focus}
  }

  getCompletions (text :string) :M.Completion[] {
    return [] // TODO
  }
}

class TermExprSpan extends TreeSpan {
  constructor (
    root :T.DefTree,
    path :T.Path,
    scope :S.Scope,
    readonly sourceText :string,
    readonly styles :string[]
  ) { super(root, path, scope) }

  get editPlaceHolder () { return "<expr>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    console.log(`termExprEdit ${key} @ ${text}`)
    switch (key) {
    case "Enter":
    case "Tab":
    case " ":
      return this.commitEdit(text, comp)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  editTree (edit :(te :T.TreeEditor) => void, fsuff? :number[]) :M.EditAction {
    const tree = this.root.edit(this.path, edit)
    return fsuff ? {tree, focus: this.path.concat(fsuff)} : {tree}
  }

  commitEdit (text :string, comp :M.Completion|void) :M.EditAction {
    // if the text is a literal, complete that (TODO: this should be via the completion; when
    // generating a list of completions for text that matches a literal, we should make the first
    // (and only) completion a tree that displays the literal with its appropriate (expected?) type)
    const asLit = C.parseLit(text)
    if (asLit !== undefined) return this.editTree(te => te.setLit(asLit))

    switch (text) {
    case "let":
      return this.editTree(te => te.setLetH(), [0])
    case "case":
      return this.editTree(te => te.setMatchH(), [0])
    case "if":
      return this.editTree(te => te.setIfH(), [0])
    case "":
      return this.editTree(te => te.setHole(TP.hole))
    }

    if (comp instanceof SymbolCompletion) return this.editTree(te => te.setRef(comp.item))
    else /*TODO*/ return this.editTree(te => te.setRef(te.tree.scope.lookupTerm(text)))
  }

  getCompletions (text :string) :M.Completion[] {
    const pred = (sym :S.Symbol) => sym.kind === "term" || sym.kind === "func"
    return this.scope.getCompletions(pred, text).map(sym => new SymbolCompletion(sym))
  }
}
