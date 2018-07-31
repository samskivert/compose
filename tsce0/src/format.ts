import * as M from "./markup"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"
import * as C from "./constants"
import { Name } from './names'

export function format (
  tree :T.DefTree, focus? :T.Path, showSigs :boolean = false
) :{elem :M.Elem, path :M.Path} {
  // console.log(`format ${tree} @ ${focus}`)
  return new Acc(tree, focus, showSigs).appendTree(tree.mkPath(), tree).finalize()
}

class SigAnnot {
  constructor (
    readonly start :number,
    readonly length :number,
    readonly depth :number,
    readonly sig :string
  ) {}
}

const symStyle = (sym :S.Symbol) => sym.flavor === "none" ? sym.kind : sym.flavor

class Acc {
  line :M.Span[] = []
  lineWidth = 0
  sigAnnots :SigAnnot[] = []
  block :M.Elem[] = []
  markupPath :M.Path = M.emptyPath
  depth = -1

  constructor (readonly root :T.DefTree, readonly focus :T.Path|void, readonly showSigs :boolean) {}

  appendTree (path :T.Path, tree :T.Tree) :this {
    this.depth += 1
    const start = this.lineWidth

    if (tree instanceof T.FunDefTree) {
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(path.x("sym"), tree.scope, tree.sym)
      this.appendSepSpan(" ")
      this.appendTree(path.x("body"), tree.body)

    } else if (tree instanceof T.TypeDefTree) {
      if (this.depth == 0) this.appendKeySpan("type ")
      const body = tree.body
      const bodyPath = path.x("body")
      this.appendTypeDefSpan(path.x("sym"), tree.scope, tree.sym)
      if (body instanceof T.TAbsTree) {
        this.appendTAbs(bodyPath, body, 0)
      } else {
        this.appendKeySpan(" = ")
        this.appendTree(bodyPath, body)
      }

    } else if (tree instanceof T.DefHoleTree) {
      this.appendSpan(new DefHoleSpan(this.root, path, tree.scope), path.x("sym"))

    } else if (tree instanceof T.CtorTree) {
      this.depth += 1
      this.appendTypeDefSpan(path.x("sym"), tree.scope, tree.sym)
      this.depth -= 1
      this.appendTree(path.x("prod"), tree.prod)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.EmptyTree) {
      // nothing (TODO?)

    } else if (tree instanceof T.THoleTree) {
      this.appendTypeExprSpan(path, tree.scope, new S.TypeHoleSym(), tree.sig)

    } else if (tree instanceof T.TConstTree) {
      this.appendTypeExprSpan(path, tree.scope, new S.TypeConstSym(tree.cnst), tree.sig)

    } else if (tree instanceof T.ArrowTree) {
      this.appendTree(path.x("from"), tree.from)
      this.appendKeySpan(" → ")
      this.appendTree(path.x("to"), tree.to)

    // } else if (tpe instanceof T.ScalarDef) {
    //   this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)

    } else if (tree instanceof T.TRefTree) {
      this.appendTypeExprSpan(path, tree.scope, tree.sym, tree.sig)

    } else if (tree instanceof T.TAbsTree) {
      this.appendTAbs(path, tree, 0)

    } else if (tree instanceof T.TAppTree) {
      this.appendTree(path.x("ctor"), tree.ctor)
      // this.appendSepSpan("[")
      this.appendSepSpan(" ")
      this.appendTree(path.x("arg"), tree.arg)
      // this.appendSepSpan("]")
      this.appendKindAnnot(start, tree.sig.kind)

    } else if (tree instanceof T.FieldTree) {
      this.appendTermDefSpan(path.x("sym"), tree.scope, tree.sym)
      this.appendAnnType(path.x("type"), tree.type)

    } else if (tree instanceof T.ProdTree) {
      for (let ii = 0; ii < tree.fields.length; ii += 1) {
        // TODO: newline separate if documented?
        // this.appendSepSpan(ii == 0 ? "(" : ",")
        this.appendSepSpan(" ")
        this.appendTree(path.x(`${ii}`), tree.fields[ii])
      }
      // if (tree.branches.length > 0) this.appendSepSpan(")")

    } else if (tree instanceof T.SumTree) {
      for (let ii = 0; ii < tree.cases.length; ii += 1) {
        this.newLine()
        this.appendSubTree(path.x(`${ii}`), tree.cases[ii])
      }

    } else if (tree instanceof T.LitTree) {
      this.appendExprSpan(path, tree.scope, new S.TermConstSym(tree.cnst), tree.sig)

    } else if (tree instanceof T.RefTree) {
      this.appendExprSpan(path, tree.scope, tree.sym, tree.sig)

    } else if (tree instanceof T.HoleTree) {
      this.appendExprSpan(path, tree.scope, new S.TermHoleSym(), tree.sig)

    } else if (tree instanceof T.PatTree) {
      this.appendPatTree(path, tree)

    } else if (tree instanceof T.AppTree) {
      this.appendTree(path.x("fun"), tree.fun)
      this.appendSepSpan(" ")
      const wantParens = (tree.fun.kind !== "inapp" && tree.arg.kind === "app")
      if (wantParens) this.appendSepSpan("(")
      this.appendTree(path.x("arg"), tree.arg)
      if (wantParens) this.appendSepSpan(")")
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.InAppTree) {
      const wantParens = (tree.arg instanceof T.AppTree && tree.arg.fun.kind === "inapp")
      if (wantParens) this.appendSepSpan("(")
      this.appendTree(path.x("arg"), tree.arg)
      if (wantParens) this.appendSepSpan(")")
      this.appendSepSpan(" ")
      this.appendTree(path.x("fun"), tree.fun)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.LetTree) {
      this.appendKeySpan("let ")
      this.appendTermDefSpan(path.x("sym"), tree.scope, tree.sym)
      this.appendAnnType(path.x("type"), tree.type)
      this.appendKeySpan(" = ")
      this.appendTree(path.x("body"), tree.body)
      this.appendTypeAnnot(start, tree.sig)
      this.newLine()
      this.appendKeySpan("in ")
      this.appendTree(path.x("expr"), tree.expr)

    } else if (tree instanceof T.LetFunTree) {
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(path.x("sym"), tree.scope, tree.sym)
      this.appendSepSpan(" ")
      this.appendTree(path.x("body"), tree.body)
      this.appendTypeAnnot(start, tree.sig)
      this.newLine()
      this.appendTree(path.x("expr"), tree.expr)

    } else if (tree instanceof T.AscTree) {
      this.appendTree(path.x("expr"), tree.expr)
      this.appendSepSpan(":")
      this.appendTree(path.x("type"), tree.type)

    } else if (tree instanceof T.AllTree) {
      this.appendAll(path, tree, 0)

    } else if (tree instanceof T.AbsTree) {
      this.appendAbs(path, tree, 0)

    } else if (tree instanceof T.IfTree) {
      this.appendKeySpan("if ")
      this.appendTree(path.x("test"), tree.test)
      this.appendKeySpan(" then ")
      this.appendTree(path.x("texp"), tree.texp)
      this.appendKeySpan(" else ")
      this.appendTree(path.x("fexp"), tree.fexp)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.MatchTree) {
      this.appendKeySpan("case ")
      this.appendTree(path.x("scrut"), tree.scrut)
      this.appendKeySpan(" of")
      this.appendTypeAnnot(start, tree.sig)
      for (let ii = 0; ii < tree.cases.length; ii += 1) {
        this.newLine()
        this.appendSubTree(path.x(`${ii}`), tree.cases[ii])
      }

    } else if (tree instanceof T.CaseTree) {
      this.appendTree(path.x("pat"), tree.pat)
      this.appendKeySpan(" → ")
      this.appendTree(path.x("body"), tree.body)
      this.appendTypeAnnot(start, tree.sig)

    } else {
      throw new Error(`Unexpected tree: ${tree}`)
    }

    this.depth -= 1
    return this
  }

  appendPatTree (path :T.Path, tree :T.PatTree) {
    if (tree instanceof T.PHoleTree) {
      this.appendPatSpan(path, tree.scope, "", tree.prototype, "term")

    } else if (tree instanceof T.PLitTree) {
      this.appendPatSpan(path, tree.scope, tree.cnst.value, tree.prototype, "cnst")

    } else if (tree instanceof T.PBindTree) {
      this.appendPatSpan(path.x("sym"), tree.scope, tree.sym.name, tree.prototype, tree.sym.kind)

    } else if (tree instanceof T.PDtorTree) {
      this.appendPatSpan(path, tree.scope, tree.ctor.name, tree.prototype, tree.ctor.kind)

    } else if (tree instanceof T.PAppTree) {
      this.appendTree(path.x("fun"), tree.fun)
      this.appendSepSpan(" ")
      this.appendTree(path.x("arg"), tree.arg)

    } else {
      throw new Error(`Unexpected pat tree: ${tree}`)
    }
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
    } else if (this.focus && path.equals(this.focus)) {
      this.appendSepSpan(":")
      this.appendTypeExprSpan(path, tree.scope, new S.TypeHoleSym(), tree.sig)
    } // otherwise append nothing
  }

  // Coalescing abs:
  // - abs inspects body: if it's another abs, append the arg & 'lift' the body
  appendAbs (path :T.Path, tree :T.AbsTree, pos :number) {
    const bodyPath = path.x("body"), body = tree.body
    this.appendTermDefSpan(path.x("sym"), tree.scope, tree.sym)
    this.appendAnnType(path.x("type"), tree.type)
    if (body instanceof T.AbsTree) {
      this.appendKeySpan(" → ")
      this.appendAbs(bodyPath, body, pos+1)
    } else if (body instanceof T.AscTree) {
      this.appendKeySpan(" → ")
      this.appendTree(bodyPath.x("type"), body.type)
      this.appendKeySpan(" = ")
      this.newLine()
      this.appendSubTree(bodyPath.x("expr"), body.expr)
    } else {
      this.appendKeySpan(" = ")
      this.appendSubTree(bodyPath, body)
    }
  }

  appendAll (path :T.Path, tree :T.AllTree, pos :number) {
    const sym = tree.sym, body = tree.body
    const bodyPath = path.x("body")
    this.appendKeySpan("∀")
    this.appendTypeDefSpan(path.x("sym"), tree.scope, sym)
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
    const bodyPath = path.x("body")
    this.appendKeySpan(" ∀")
    this.appendTypeDefSpan(path.x("sym"), tree.scope, sym)
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
    // if (path) console.log(`appendSpan ${span.displayText} @ ${path} (focus: ${this.focus})`)
    // if we're appending our target tree node, capture the path to it
    if (path && this.focus && path.equals(this.focus)) this.markupPath =
      M.mkPath([this.block.length], this.line.length)
    this.line.push(span)
    this.lineWidth += span.displayText.length
  }

  appendKeySpan (text :string) {
    this.appendSpan(new M.TextSpan(text, ["keyword"]))
  }
  appendSepSpan (text :string) {
    this.appendSpan(new M.TextSpan(text, ["separator"]))
  }
  appendTypeDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol) {
    let start = this.lineWidth
    this.appendSpan(new TypeDefSpan(path, scope, sym), path)
    this.appendKindAnnot(start, sym.type.kind)
  }
  appendTypeExprSpan (path :T.Path, scope :S.Scope, sym :S.Symbol, type :TP.Type) {
    let start = this.lineWidth
    this.appendSpan(new TypeExprSpan(path, scope, sym), path)
    this.appendKindAnnot(start, type.kind)
  }
  appendTermDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol) {
    let start = this.lineWidth
    this.appendSpan(new TermDefSpan(path, scope, sym), path)
    this.appendTypeAnnot(start, sym.type)
  }
  appendExprSpan (path :T.Path, scope :S.Scope, sym :S.Symbol, type :TP.Type) {
    let start = this.lineWidth
    this.appendSpan(new TermExprSpan(path, scope, sym), path)
    this.appendTypeAnnot(start, type)
  }
  appendPatSpan (path :T.Path, scope :S.Scope, name :string, type :TP.Type, style :string) {
    let start = this.lineWidth
    this.appendSpan(new PatSpan(path, scope, name, style, type), path)
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

class NameCompletion extends M.Completion {
  constructor (readonly text :string) { super() }
  get name () :string { return this.text }
  get type () :TP.Type { return TP.hole }
  display () :M.Line { return new M.Line([new M.TextSpan(this.text)], []) }
  apply (te :T.TreeEditor) { te.setName(this.text) }
  equals (that :M.Completion) :boolean {
    return that instanceof NameCompletion && this.text === that.text
  }
}

function formatCompletion (name :string, kind :S.Kind, sig :TP.Type) :M.Line {
  const sigStyle = kind == "type" ? "kind" : "type"
  return new M.Line([new M.TextSpan(name, [kind]),
                     new M.TextSpan(` :`),
                     new M.TextSpan(sig.toString(), [sigStyle])], []) // TODO: better formating
}

abstract class SymbolCompletion extends M.Completion {
  constructor (readonly item :S.Symbol) { super() }
  get name () :Name { return this.item.name }
  get type () :TP.Type { return this.item.type }
  display () :M.Line { return formatCompletion(this.item.name, this.item.kind, this.item.type) }
  equals (that :M.Completion) :boolean {
    return that instanceof SymbolCompletion && this.item === that.item
  }
}

class TermSymbolCompletion extends SymbolCompletion {
  apply (te :T.TreeEditor) { te.setRef(this.item) }
}

// TODO: better formating
class TypeSymbolCompletion extends SymbolCompletion {
  apply (te :T.TreeEditor) { te.setTRef(this.item) }
}

class TermHoleCompletion extends M.Completion {
  get name () :Name { return "?" }
  get type () :TP.Type { return TP.hole }
  display () { return new M.Line([new M.TextSpan("?")], []) }
  apply (te :T.TreeEditor) { te.setHole() }
  equals (that :M.Completion) { return that instanceof TermHoleCompletion }
}
const termHoleCompletion = new TermHoleCompletion()

class TypeHoleCompletion extends M.Completion {
  get name () :Name { return "?" }
  get type () :TP.Type { return TP.hole }
  display () { return new M.Line([new M.TextSpan("?")], []) }
  apply (te :T.TreeEditor) { te.setTHole() }
  equals (that :M.Completion) { return that instanceof TypeHoleCompletion }
}
const typeHoleCompletion = new TypeHoleCompletion()

abstract class ConstCompletion extends M.Completion {
  constructor (readonly cnst :C.Constant) { super() }
  get name () :Name { return this.cnst.value } // TODO: is there a better lie?
  get type () :TP.Type { return new TP.Const(this.cnst) }
  display () :M.Line {
    return new M.Line([new M.TextSpan(this.cnst.value, ["constant"])], [])
  }
  equals (that :M.Completion) :boolean {
    return that instanceof ConstCompletion && this.cnst.equals(that.cnst)
  }
}
class TermConstCompletion extends ConstCompletion {
  apply (te :T.TreeEditor) { te.setLit(this.cnst) }
}
class PatConstCompletion extends ConstCompletion {
  apply (te :T.TreeEditor) { te.setPLit(this.cnst) }
}

type Rule = {
  name: string
  key: (ev :M.KeyEvent) => boolean
  apply: (path :T.Path, comp :M.Completion) => T.DefTree|void
  focusOp? :(path :T.Path) => T.Path
}

const addArgType :Rule = {
  name: "addArgType",
  key: ev => ev.key === ":",
  apply: (path, comp) => {
    if (path.endsWith("abs", "sym")) {
      comp.edit(path)
      const typePath = path.pop().x("type")
      if (typePath.selected instanceof T.EmptyTree) {
        typePath.edit(te => te.setTHole())
      }
      return path.root
    }
    return undefined
  },
  focusOp: path => path.sib("type")
}

const isEmptyAbs = (tree :T.Tree) => tree instanceof T.AbsTree && tree.sym.name === ""
const isEmptyTAbs = (tree :T.Tree) => tree instanceof T.TAbsTree && tree.sym.name === ""
const isEmptyAll = (tree :T.Tree) => tree instanceof T.AbsTree && tree.sym.name === ""

const addAbs :Rule = {
  name: "addAbs",
  key: ev => ev.key === " " && !ev.shiftKey,
  apply: (path, comp) => {
    if (path.endsWith("abs", "sym") || path.endsWith("fundef", "sym")) {
      let {tree} = comp.edit(path)
      const bodyPath = path.sib("body"), body = bodyPath.selected as T.Tree
      return isEmptyAbs(body) ? tree : bodyPath.edit(te => te.setAbs("").adopt("body", body))
    }
    return undefined
  },
  focusOp: path => path.sib("body").x("sym")
}

const addTAbs :Rule = {
  name: "addTAbs",
  key: ev => ev.key === " ",
  apply: (path, comp) => {
    if (path.endsWith("typedef", "sym") || path.endsWith("tabs", "sym")) {
      let {tree} = comp.edit(path)
      const bodyPath = path.sib("body"), body = bodyPath.selected as T.Tree
      return isEmptyTAbs(body) ? tree : bodyPath.edit(te => te.setTAbs("").adopt("body", body))
    }
    return undefined
  },
  focusOp: path => path.sib("body").x("sym")
}

const addAll :Rule = {
  name: "addAll",
  key: ev => ev.key === " " && !ev.shiftKey,
  apply: (path, comp) => {
    if (path.endsWith("fundef", "sym") || path.endsWith("all", "sym")) {
      let {tree} = comp.edit(path)
      const bodyPath = path.sib("body"), body = bodyPath.selected as T.Tree
      return isEmptyAll(body) ? tree : bodyPath.edit(te => te.setAll("").adopt("body", body))
    }
    return undefined
  },
  focusOp: path => path.sib("body").x("sym")
}

function findBody (tree :T.Tree, path :T.Path) :T.Path {
  // TODO: maybe aggregate these into some shared abstract base class...
  if (tree instanceof T.TypeDefTree ||
      tree instanceof T.TAbsTree ||
      tree instanceof T.FunDefTree ||
      tree instanceof T.AllTree ||
      tree instanceof T.AbsTree) return findBody(tree.body, path.x("body"))
  else return path
}

const eqToBody :Rule = {
  name: "eqToBody",
  key: ev => ev.key === "=",
  apply: (path, comp) => {
    if (path.endsWith("typedef", "sym") ||
        path.endsWith("tabs", "sym") ||
        path.endsWith("fundef", "sym") ||
        path.endsWith("all", "sym") ||
        path.endsWith("abs", "sym")) {
      return comp.edit(path).tree
    }
    return undefined
  },
  focusOp: path => findBody(path.selectedParent, path.pop()).firstEditable()
}

function editAnd (path :T.Path, editfn :T.EditFn, ...fsuff :string[]) :M.EditAction {
  const tree = path.edit(editfn)
  return fsuff.length > 0 ? {tree, focus: path.concat(fsuff)} : {tree}
}

function insertCaseAt (matchPath :T.Path, caseIdx :number) :M.EditAction {
  const matchTree = matchPath.selected as T.MatchTree
  // shift all the existing cases "down" one branch
  for (let ii = matchTree.cases.length; ii > caseIdx; ii -= 1) {
    matchTree.adopt(`${ii}`, matchTree.cases[ii-1])
  }
  const caseNPath = matchPath.x(`${caseIdx}`)
  const tree = caseNPath.edit(te => te.setCaseHole())
  return {tree, focus: caseNPath.x("pat")}
}

abstract class TreeSpan extends M.EditableSpan {
  constructor(readonly path :T.Path, readonly scope :S.Scope) { super() }

  get isHole () :boolean {
    const branch = this.path.selected
    if (branch instanceof T.Tree && branch.isHole) return true
    if (branch instanceof S.Symbol && branch.name === "") return true
    return false
  }

  insertHole (dir :M.Dir) :M.EditAction|void {
    switch (dir) {
    case M.Dir.Left:
      if (this.path.endsWith("match", "scrut")) {
        // TODO: wrap the match in an app
        return undefined
      }
      if (this.path.endsWith("app", "arg")) {
        // wrap the app fun in an app + hole
        const funPath = this.path.sib("fun")
        const tree = funPath.edit(te => {
          const oldFun = te.currentTree
          te.setApp().editBranches({
            "fun": fun => fun.adopt(oldFun),
            "arg": arg => arg.setHole()
          })
        })
        return {tree, focus: funPath.x("arg")}
      }
      break

    case M.Dir.Right:
      if (this.path.endsWith("app", "arg")) {
        // wrap the app in an app + hole
        const appPath = this.path.pop()
        const tree = appPath.edit(te => {
          const oldApp = te.currentTree
          te.setApp().editBranches({
            "fun": fun => fun.adopt(oldApp),
            "arg": arg => arg.setHole()
          })
        })
        return {tree, focus: appPath.x("arg")}
      }
      break

    case M.Dir.Up:
      // if we're in a match case, add a case above
      const upCasePath = this.path.popTo("case")
      if (upCasePath) {
        const matchTree = upCasePath.selectedParent as T.MatchTree
        const caseIdx = matchTree.cases.indexOf(upCasePath.selected as T.CaseTree)
        return insertCaseAt(upCasePath.pop(), caseIdx)
      }
      break

    case M.Dir.Down:
      // if we're in the scrut of a match, add a case at position 0
      if (this.path.endsWith("match", "scrut")) {
        return insertCaseAt(this.path.pop(), 0)
      }
      // if we're in a match case, add a case below
      const dnCasePath = this.path.popTo("case")
      if (dnCasePath) {
        const matchTree = dnCasePath.selectedParent as T.MatchTree
        const caseIdx = matchTree.cases.indexOf(dnCasePath.selected as T.CaseTree)
        return insertCaseAt(dnCasePath.pop(), caseIdx+1)
      }
      break
    }
  }

  toString () { return `${this.displayText} @ ${this.path}` }
}

class DefHoleSpan extends TreeSpan {
  get isHole () :boolean { return true }
  get sourceText () { return "" }
  get styles () { return ["keyword"] }
  get displayPlaceHolder () { return "<new def>" }
  get editPlaceHolder () { return "<kind>" }

  constructor (readonly root :T.DefTree, path :T.Path, scope :S.Scope) { super(path, scope) }

  handleKey (ev :M.KeyEvent, text :string, comp :M.Completion|void) :M.EditAction|void {
    console.log(`defHoleEdit ${ev} @ ${text}`)
    switch (ev.key) {
    case "Enter":
    case "Tab":
    case " ":
      return this.commitEdit(text, comp)
    }
  }

  commitEdit (text :string, comp :M.Completion|void) :M.EditAction|void {
    const msym = this.root.owner as S.ModuleSym
    if (text === "fun") {
      const tree = T.mkFunDef(msym, "").editBranch(
        "body", body => body.setAbs("").editBranch(
          "body", body => body.setHole()))
      return {tree, focus: tree.mkPath("sym")}
    }
    else if (text === "sum") {
      const tree = T.mkTypeDef(msym, "").editBranch(
        "body", body => body.setSum().editBranch(
          "0", zero => zero.setCtor("").editBranch(
            "prod", prod => prod.setProd())))
      return {tree, focus: tree.mkPath("sym")}
    }
    else if (text === "prod") {
      const tree = T.mkTypeDef(msym, "").editBranch(
        "body", body => body.setCtor("").editBranch(
          "prod", prod => prod.setProd().editBranch(
            "0", zero => zero.setField("").editBranch(
              "type", type => type.setTHole()))))
      return {tree, focus: tree.mkPath("sym")}
    // } else if (text === "let") {
      // TODO
    }
  }
}

abstract class RuleSpan extends TreeSpan {

  abstract get rules () :Rule[]

  handleKey (ev :M.KeyEvent, text :string, compM :M.Completion|void) :M.EditAction|void {
    const {path, rules} = this, comp = compM || this.createDefaultComp(text)

    for (let rule of rules) {
      if (rule.key(ev)) {
        const tree = rule.apply(path, comp)
        if (tree) {
          console.log(`FIRED ${rule.name}`)
          const focus = rule.focusOp && rule.focusOp(path)
          return {tree, focus}
        }
      }
    }

    switch (ev.key) {
    case "Enter":
    case "Tab":
      console.log(`Commiting edit: ${comp}`)
      return comp.edit(path)
    }
  }

  protected abstract createDefaultComp (text :string) :M.Completion
}

const typeRules :Rule[] = [
  addArgType,
  addAbs,
  addTAbs,
  addAll,
  eqToBody,
]

class TypeDefSpan extends RuleSpan {
  constructor (path :T.Path, scope :S.Scope, readonly sym :S.Symbol) { super(path, scope) }

  get rules () { return typeRules }
  get styles () { return ["type"] }
  get sourceText () { return this.sym.name }
  get displayText () { return this.sym.displayName }
  get editPlaceHolder () { return "<name>" }

  getCompletions (text :string) :M.Completion[] { return [] } // TODO
  protected createDefaultComp (text :string) { return new NameCompletion(text) }
}

const termRules :Rule[] = [
  addArgType,
  addAbs,
  addTAbs,
  addAll,
  eqToBody,
]

class TermDefSpan extends RuleSpan {
  constructor (path :T.Path, scope :S.Scope, readonly sym :S.Symbol) { super(path, scope) }

  get rules () { return termRules }
  get styles () { return [symStyle(this.sym)] }
  get sourceText () { return this.sym.name }
  get displayText () { return this.sym.displayName }
  get editPlaceHolder () { return "<name>" }

  getCompletions (text :string) :M.Completion[] { return [] } // TODO
  protected createDefaultComp (text :string) { return new NameCompletion(text) }
}

class TypeExprSpan extends TreeSpan {
  constructor (path :T.Path, scope :S.Scope, readonly sym :S.Symbol) { super(path, scope) }

  get styles () { return ["type"] }
  get sourceText () { return this.sym.name }
  get displayText () { return this.sym.displayName }
  get editPlaceHolder () { return "<type>" }

  getCompletions (text :string) :M.Completion[] {
    return this.scope.getCompletions(sym => sym.kind === "type", text).
      map(sym => new TypeSymbolCompletion(sym))
  }

  handleKey (ev :M.KeyEvent, text :string, compM :M.Completion|void) :M.EditAction|void {
    switch (ev.key) {
    case "Enter":
    case " ":
      return this.commitEdit(text, compM, ev.key === " ")
    case "Tab":
      // TODO: only apply the completion if I performed some action that indicates that I want the
      // completion (typing at least one character?)
      return this.commitEdit(text, compM, false)
    // case "=":
    //   // TODO: do we want to allow = in type names?
    //   if (termBodyPath) {
    //     const {tree} = comp.apply(path)
    //     return {tree, focus: termBodyPath}
    //   }
    //   else return comp.apply(path)
    // case " ":
    //   if (typeBodyPath) {
    //     comp.apply(path)
    //     return this.edit(typeBodyPath, te => {
    //       const body = te.currentTree
    //       return te.setTAbs("").adopt("body", body)
    //     })
    //   }
    //   // TODO: stop the instanity, refactor all this to use composable rules
    //   else if (termBodyPath) {
    //     let tree = comp.apply(path)
    //     // if the body is an empty abs, we want to simply start editing it
    //     // if not, we want to insert an empty abs first (and then edit that)
    //     let child = termBodyPath.selected
    //     if (!(child instanceof T.AbsTree) || child.sym.name !== "") {
    //       tree = termBodyPath.edit(te => {
    //         const oldBody = te.currentTree
    //         return (ev.shiftKey ? te.setTAbs("").adopt("body", oldBody) :
    //                 te.setAbs("").adopt("body", oldBody))
    //       })
    //     }
    //     return ({tree, focus: termBodyPath.x("sym")})
    //   }
    //   else comp.apply(path)
    }
  }

  commitEdit (text :string, compM :M.Completion|void, viaSpace :boolean) :M.EditAction {
    const path = this.path
    const comp = compM || (text === "" ? typeHoleCompletion :
                           new TypeSymbolCompletion(path.selectedParent.scope.lookupType(text)))
    let compKind = comp.type.kind

    // if we're setting a non-constructor, then no funny business
    if (!(compKind instanceof TP.KArrow)) {
      // TODO: do we eventually want to handle inapp ctors?
      return comp.edit(path)
    }

    // if we're replacing the ctor in an existing app, then don't fool with the args
    // (TODO: if it takes more args, we could insert holes? what about fewer? seems fiddly)
    if (!path.selectsHole) {
      return comp.edit(path)
    }

    // otherwise we're sticking a type ctor somewhere new, so add holes for all args
    let edit = (te :T.TreeEditor) => comp.apply(te)
    while (compKind instanceof TP.KArrow) {
      const inner = edit
      edit = te => te.setTApp().editBranches({"ctor": inner, "arg": arge => arge.setTHole()})
      compKind = compKind.res
    }
    return editAnd(path, edit)
  }
}

class TmplCompletion extends M.Completion {
  get name () :Name { return this.disp } // TODO: this is a terrible lie...
  get type () :TP.Type { return  TP.hole }
  get isTemplate () :boolean { return true }
  constructor (readonly disp :string, readonly editfn :T.EditFn,
               readonly psuff :string[]) { super() }
  display () { return new M.Line([new M.TextSpan(this.disp)], []) }
  apply (te :T.TreeEditor) { this.editfn(te) }
  edit (path :T.Path) :M.EditAction { return editAnd(path, te => this.apply(te), ...this.psuff) }
  equals (that :M.Completion) :boolean {
    return that instanceof TmplCompletion && this.disp === that.disp }
}

const letCompletion = new TmplCompletion("let ? = ? in ?", te => te.setLet("").editBranches({
  "body": body => body.setHole(),
  "expr": expr => expr.setHole(),
}), ["sym"])

const caseCompletion = new TmplCompletion("case ? of ?", te => te.setMatch().editBranches({
  "scrut": scrut => scrut.setHole(),
  "0": case0 => case0.setCaseHole()
}), ["scrut"])

const ifCompletion = new TmplCompletion("if ? then ? else ?", te => te.setIf().editBranches({
  "test": test => test.setHole(),
  "texp": texp => texp.setHole(),
  "fexp": fexp => fexp.setHole()
}), ["test"])

class TermExprSpan extends TreeSpan {
  constructor (path :T.Path, scope :S.Scope, readonly sym :S.Symbol) { super(path, scope) }

  get styles () { return [symStyle(this.sym)] }
  get sourceText () { return this.sym.name }
  get displayText () { return this.sym.displayName }
  get editPlaceHolder () { return "<expr>" }

  getCompletions (text :string) :M.Completion[] {
    // if the text is a literal, return a single completion which is that literal
    const asLit = C.parseLit(text)
    if (asLit !== undefined) return [new TermConstCompletion(asLit)]

    // otherwise find a symbol that completes the text & matches the desired type
    const sel = this.path.selected, selType = sel instanceof T.Tree ? sel.prototype : TP.hole
    console.log(`getCompletions '${text}' :: ${selType}`)
    const pred = (sym :S.Symbol) => {
      if (sym.kind === "term") console.log(` check comp ${sym} :: ${sym.type} :: ${sym.type.constructor.name} :: ${sym.type.join(selType)}`)
      return (sym.kind === "term") && !sym.type.join(selType).isError
    }
    const comps :M.Completion[] = this.scope.getCompletions(pred, text).
      map(sym => new TermSymbolCompletion(sym))

    // if the text starts with a tree-stub generator, include that option as well
    if (text.length > 1 && "let".startsWith(text)) comps.unshift(letCompletion)
    if (text.length > 1 && "case".startsWith(text)) comps.unshift(caseCompletion)
    if (text === "if") comps.unshift(ifCompletion)
    return comps
  }

  handleKey (ev :M.KeyEvent, text :string, comp :M.Completion|void) :M.EditAction|void {
    console.log(`termExprEdit ${ev} @ ${text}`)
    switch (ev.key) {
    case "Enter":
    case "Tab":
    case " ":
      return this.commitEdit(text, comp, ev.key === " ")
    }
  }

  commitEdit (text :string, compM :M.Completion|void, viaSpace :boolean) :M.EditAction {
    const path = this.path
    // if we didn't get a completion, fake it by looking up the text as a symbol (TODO...)
    const comp = compM || (text === "" ? termHoleCompletion :
                           new TermSymbolCompletion(path.selectedParent.scope.lookupTerm(text)))
    let compType = comp.type

    // if we're setting a non-function, use our setValue helper
    if (!(compType instanceof TP.Arrow)) {
      if (!viaSpace || comp.isTemplate) return comp.edit(path)
      // if a non-fun is entered and then space is pressed, we want to create an inapp with this
      // value as the arg, and move the focus to the fun
      else return editAnd(path, te => te.setInApp().editBranches({
        "arg": arg => comp.apply(arg),
        "fun": fun => fun.setHole()
      }), "fun")
    }

    // TODO: only do stub arg insertion and inapp conversion when replacing a HOLE

    // if we're setting the fun of an inapp, add holes in for the remaining args
    const lastIdx = path.length-1
    console.log(`Inserting fun ${path.kindAt(lastIdx-1)} ${path.id(lastIdx)}`)
    if (path.kindAt(lastIdx-1) === "inapp" && path.id(lastIdx) === "fun" && path.selectsHole) {
      // first insert the ref into the current span hole
      comp.edit(path)
      // now pop up a level and wrap this whole inapp in an app with a hole for an arg
      return editAnd(path.pop(), te => {
        const oldInApp = te.currentTree
        te.setApp().editBranches({
          "fun": fun => fun.adopt(oldInApp),
          "arg": arg => arg.setHole()
        })
      }, "arg")
    }

    // if we're replacing the fun in an existing app, then don't fool with the args
    // (TODO: if it takes more args, we could insert holes? what about fewer? seems fiddly)
    if (!path.selectsHole) {
      return comp.edit(path)
    }

    // otherwise we're sticking a fun somewhere new, so add holes for all args
    let edit = (te :T.TreeEditor) => comp.apply(te)
    while (compType instanceof TP.Arrow) {
      const inner = edit
      edit = te => te.setApp().editBranches({"fun": inner, "arg": arge => arge.setHole()})
      compType = compType.res
    }
    return editAnd(path, edit)
  }
}

class PatBindCompletion extends M.Completion {
  constructor (readonly text :string, readonly type :TP.Type) { super() }
  get name () :Name { return this.text }
  display () :M.Line { return formatCompletion(this.text, "term", this.type) }
  apply (te :T.TreeEditor) { te.setPBind(this.text) }
  equals (that :M.Completion) :boolean {
    return that instanceof PatBindCompletion && this.text === that.text
  }
}

const patRules :Rule[] = [
  addArgType,
  addAbs,
  addTAbs,
  addAll,
  eqToBody, // TODO: change arrow to =, eqToBody moves to expr?
]

class PatDtorCompletion extends SymbolCompletion {
  apply (te :T.TreeEditor) {
    let edit = (te :T.TreeEditor) => te.setPDtor(this.item)
    let dtorType = this.item.type.skolemize(new Map())
    while (dtorType instanceof TP.Arrow) {
      const inner = edit
      edit = te => te.setPApp().editBranches({"fun": inner, "arg": arge => arge.setPHole()})
      dtorType = dtorType.res
    }
    edit(te)
  }
}

class PatSpan extends RuleSpan {
  constructor (path :T.Path, scope :S.Scope,
               readonly sourceText :string,
               readonly style :string,
               readonly type :TP.Type) { super(path, scope) }

  get rules () { return patRules }
  get styles () { return [this.style] }
  get editPlaceHolder () { return "<pat>" }

  getCompletions (text :string) :M.Completion[] {
    // if the text is a literal, return a single completion which is that literal
    const asLit = C.parseLit(text)
    if (asLit !== undefined) return [new PatConstCompletion(asLit)]

    console.log(`getCompletions for pat ${this.type}`)

    // otherwise find a symbol that completes the text & matches the desired type
    const pred = (sym :S.Symbol) => (sym.kind === "term" && sym.flavor === "ctor" &&
                                     TP.patApplies(sym.type, this.type))
    const comps :M.Completion[] = this.scope.getCompletions(pred, text).
      map(sym => new PatDtorCompletion(sym))

    if (comps.every(c => c.name !== text)) {
      comps.unshift(this.createDefaultComp(text))
    }
    return comps
  }

  protected createDefaultComp (text :string) { return new PatBindCompletion(text, this.type) }
}
