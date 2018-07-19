import * as M from "./markup"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"
import * as C from "./constants"

export function format (
  tree :T.DefTree, focus :T.Path, showSigs :boolean = false
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
    function xpath (id :string) { return T.extendPath(path, id) }
    const start = this.lineWidth

    if (tree instanceof T.FunDefTree) {
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(xpath("sym"), tree.scope, tree.sym, undefined, xpath("body"))
      this.appendSepSpan(" ")
      this.appendTree(xpath("body"), tree.body)

    } else if (tree instanceof T.TypeDefTree) {
      if (this.depth == 0) this.appendKeySpan("type ")
      const body = tree.body
      const bodyPath = xpath("body")
      this.appendTypeDefSpan(xpath("sym"), tree.scope, tree.sym, bodyPath)
      if (body instanceof T.TAbsTree) {
        this.appendTAbs(bodyPath, body, 0)
      } else {
        this.appendKeySpan(" = ")
        this.appendTree(bodyPath, body)
      }

    } else if (tree instanceof T.CtorTree) {
      this.depth += 1
      this.appendTypeDefSpan(xpath("sym"), tree.scope, tree.sym, xpath("prod"))
      this.depth -= 1
      this.appendTree(xpath("prod"), tree.prod)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.EmptyTree) {
      // nothing (TODO?)

    } else if (tree instanceof T.THoleTree) {
      this.appendTypeExprSpan(path, tree.scope, "", tree.sig)

    } else if (tree instanceof T.TConstTree) {
      this.appendTypeExprSpan(path, tree.scope, tree.cnst.value, tree.sig)

    } else if (tree instanceof T.ArrowTree) {
      this.appendTree(xpath("from"), tree.from)
      this.appendKeySpan(" → ")
      this.appendTree(xpath("to"), tree.to)

    // } else if (tpe instanceof T.ScalarDef) {
    //   this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)

    } else if (tree instanceof T.TRefTree) {
      this.appendTypeExprSpan(path, tree.scope, tree.sym.name, tree.sig)

    } else if (tree instanceof T.TAbsTree) {
      this.appendTAbs(path, tree, 0)

    } else if (tree instanceof T.TAppTree) {
      this.appendTree(xpath("ctor"), tree.ctor)
      // this.appendSepSpan("[")
      this.appendSepSpan(" ")
      this.appendTree(xpath("arg"), tree.arg)
      // this.appendSepSpan("]")
      this.appendKindAnnot(start, tree.sig.kind)

    } else if (tree instanceof T.FieldTree) {
      this.appendTermDefSpan(xpath("sym"), tree.scope, tree.sym)
      this.appendAnnType(xpath("type"), tree.type)

    } else if (tree instanceof T.ProdTree) {
      for (let ii = 0; ii < tree.fields.length; ii += 1) {
        // TODO: newline separate if documented?
        // this.appendSepSpan(ii == 0 ? "(" : ",")
        this.appendSepSpan(" ")
        this.appendTree(xpath(`${ii}`), tree.fields[ii])
      }
      // if (tree.branches.length > 0) this.appendSepSpan(")")

    } else if (tree instanceof T.SumTree) {
      for (let ii = 0; ii < tree.cases.length; ii += 1) {
        this.newLine()
        this.appendSubTree(xpath(`${ii}`), tree.cases[ii])
      }

    } else if (tree instanceof T.LitTree) {
      this.appendExprSpan(path, tree.scope, tree.cnst.value, tree.sig, "constant")

    } else if (tree instanceof T.RefTree) {
      this.appendExprSpan(path, tree.scope, tree.sym.name, tree.sig, tree.sym.kind)

    } else if (tree instanceof T.HoleTree) {
      if (this.depth == 0) this.appendSpan(new DefHoleSpan(this.root, path, tree.scope), xpath("sym"))
      else this.appendExprSpan(path, tree.scope, "", tree.sig, "term")

    } else if (tree instanceof T.PLitTree) {
      this.appendExprSpan(path, tree.scope, tree.cnst.value, tree.sig, "constant")

    } else if (tree instanceof T.PBindTree) {
      this.appendTermDefSpan(xpath("sym"), tree.scope, tree.sym)

    } else if (tree instanceof T.PDtorTree) {
      this.appendExprSpan(path, tree.scope, tree.ctor.name, tree.sig, tree.ctor.kind)

    } else if (tree instanceof T.PAppTree) {
      this.appendTree(xpath("fun"), tree.fun)
      this.appendSepSpan(" ")
      this.appendTree(xpath("arg"), tree.arg)

    } else if (tree instanceof T.AppTree) {
      this.appendTree(xpath("fun"), tree.fun)
      this.appendSepSpan(" ")
      const wantParens = (tree.fun.kind !== "inapp" && tree.arg.kind === "app")
      if (wantParens) this.appendSepSpan("(")
      this.appendTree(xpath("arg"), tree.arg)
      if (wantParens) this.appendSepSpan(")")
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.InAppTree) {
      this.appendTree(xpath("arg"), tree.arg)
      this.appendSepSpan(" ")
      this.appendTree(xpath("fun"), tree.fun)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.LetTree) {
      this.appendKeySpan("let ")
      const typePath = xpath("type")
      const bodyPath = xpath("body")
      this.appendTermDefSpan(xpath("sym"), tree.scope, tree.sym, typePath, bodyPath)
      this.appendAnnType(typePath, tree.type)
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, tree.body)
      this.appendTypeAnnot(start, tree.sig)
      this.newLine()
      this.appendKeySpan("in ")
      this.appendTree(xpath("expr"), tree.expr)

    } else if (tree instanceof T.LetFunTree) {
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(xpath("sym"), tree.scope, tree.sym, undefined, xpath("body"))
      this.appendSepSpan(" ")
      this.appendTree(xpath("body"), tree.body)
      this.appendTypeAnnot(start, tree.sig)
      this.newLine()
      this.appendTree(xpath("expr"), tree.expr)

    } else if (tree instanceof T.AscTree) {
      this.appendTree(xpath("expr"), tree.expr)
      this.appendSepSpan(":")
      this.appendTree(xpath("type"), tree.type)

    } else if (tree instanceof T.AllTree) {
      this.appendAll(path, tree, 0)

    } else if (tree instanceof T.AbsTree) {
      this.appendAbs(path, tree, 0)

    } else if (tree instanceof T.IfTree) {
      this.appendKeySpan("if ")
      this.appendTree(xpath("test"), tree.test)
      this.appendKeySpan(" then ")
      this.appendTree(xpath("texp"), tree.texp)
      this.appendKeySpan(" else ")
      this.appendTree(xpath("fexp"), tree.fexp)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.MatchTree) {
      this.appendKeySpan("case ")
      this.appendTree(xpath("scrut"), tree.scrut)
      this.appendKeySpan(" of")
      this.appendTypeAnnot(start, tree.sig)
      for (let ii = 0; ii < tree.cases.length; ii += 1) {
        this.newLine()
        this.appendSubTree(xpath(`${ii}`), tree.cases[ii])
      }

    } else if (tree instanceof T.CaseTree) {
      this.appendTree(xpath("pat"), tree.pat)
      this.appendKeySpan(" → ")
      this.appendTree(xpath("body"), tree.body)
      this.appendTypeAnnot(start, tree.sig)

    } else {
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

  // Coalescing abs:
  // - abs inspects body: if it's another abs, append the arg & 'lift' the body
  appendAbs (path :T.Path, tree :T.AbsTree, pos :number) {
    const argPath  = T.extendPath(path, "arg")
    const typePath = T.extendPath(path, "type")
    const bodyPath = T.extendPath(path, "body"), body = tree.body
    this.appendTermDefSpan(argPath, tree.scope, tree.sym, typePath, bodyPath)
    this.appendAnnType(typePath, tree.type)
    if (body instanceof T.AbsTree) {
      this.appendKeySpan(" → ")
      this.appendAbs(bodyPath, body, pos+1)
    } else if (body instanceof T.AscTree) {
      this.appendKeySpan(" → ")
      this.appendTree(T.extendPath(bodyPath, "type"), body.type)
      this.appendKeySpan(" = ")
      this.newLine()
      this.appendSubTree(T.extendPath(bodyPath, "expr"), body.expr)
    } else {
      this.appendKeySpan(" = ")
      this.appendSubTree(bodyPath, body)
    }
  }
  appendAll (path :T.Path, tree :T.AllTree, pos :number) {
    const sym = tree.sym, body = tree.body
    const bodyPath = T.extendPath(path, "body")
    this.appendKeySpan("∀")
    this.appendTypeDefSpan(T.extendPath(path, "sym"), tree.scope, sym, bodyPath)
    if (body instanceof T.AllTree) {
      this.appendAll(bodyPath, body, pos+1)
    } else if (body instanceof T.AbsTree) {
      this.appendSepSpan(" ")
      this.appendAbs(bodyPath, body, 0)
    } else {
      // TODO: this shouldn't really ever happen, we should complain?
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, body)
    }
  }

  appendTAbs (path :T.Path, tree :T.TAbsTree, pos :number) {
    const {sym, body} = tree
    const bodyPath = T.extendPath(path, "body")
    this.appendKeySpan(" ∀")
    this.appendTypeDefSpan(T.extendPath(path, "sym"), tree.scope, sym, bodyPath)
    if (body instanceof T.TAbsTree) {
      this.appendSepSpan(" ")
      this.appendTAbs(bodyPath, body, pos+1)
    } else if (body instanceof T.ProdTree) {
      this.appendTree(bodyPath, body)
    } else {
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
      if (this.sigAnnots.length > 0) {
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
        this.sigAnnots = []
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

  toString () { return `${this.displayText} @ ${this.path}` }
}

function mkFun (msym :S.ModuleSym) {
  return T.mkFunDef(msym, "").editBranch(
    "body", body => body.setAbs("").editBranch(
      "body", body => body.setHole(TP.hole)))
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
      return {tree: mkFun(msym), focus: ["sym"]}
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
                  .edit(bodyPath, te => te.setTAbs("").adopt("body", te.currentTree)),
        focus: bodyPath.concat(["sym"])
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
    const {root, path, bodyPath} = this
    switch (key) {
    case "Enter":
    case "Tab":
      return {tree: root.edit(path, setRef)}
    case " ":
      return bodyPath ?
        ({tree: root.edit(bodyPath, te => te.setTAbs("").adopt("body", te.currentTree))}) :
        ({tree: root.edit(path, setRef)})
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
      if (!(child instanceof T.AbsTree) || child.sym.name !== "") {
        tree = root.edit(bodyPath, te => {
          const oldBody = te.currentTree
          return (mods.shift ? te.setTAbs("").adopt("body", oldBody) :
                  te.setAbs("").adopt("body", oldBody))
        })
      }
      return ({tree, focus: bodyPath.concat(["sym"])})
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

  editTree (edit :(te :T.TreeEditor) => void, fsuff? :string[]) :M.EditAction {
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
      return this.editTree(te => te.setLetH(), ["sym"])
    case "case":
      return this.editTree(te => te.setMatchH(), ["scrut"])
    case "if":
      return this.editTree(te => te.setIfH(), ["test"])
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
