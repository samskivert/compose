import * as C from "./constants"
import * as K from "./keymap"
import * as M from "./markup"
import * as MD from "./module"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"
import { Name } from './names'

export type FormatOpts = {
  showSigs? :boolean
  editing? :boolean
}

export function format (
  mod :MD.Module, tree :T.DefTree, focus? :T.Path, opts :FormatOpts = {}
) :{elem :M.Elem, path :M.Path} {
  console.log(`format ${tree} / ${tree.sym} @ ${focus} (${JSON.stringify(opts)})`)
  return new Acc(mod, tree, focus, opts).appendTree(new T.Path(), tree).finalize()
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

  constructor (
    readonly mod :MD.Module,
    readonly root :T.DefTree,
    readonly focus :T.Path|void,
    readonly opts :FormatOpts) {}

  appendTree (path :T.Path, tree :T.Tree) :this {
    this.depth += 1
    const start = this.lineWidth, {editing} = this.opts

    this.maybeAppendError(path, tree)

    if (tree instanceof T.EmptyTree) {
      // nothing (TODO?)

    } else if (tree instanceof T.FunDefTree) {
      this.appendKeySpan("fun ")
      this.appendSymSpan(new TermDefSpan(this.root, path.x("sym"), tree))
      this.appendSepSpan(" ")
      this.appendTree(path.x("body"), tree.body)

    } else if (tree instanceof T.TypeDefTree) {
      if (this.depth == 0) this.appendKeySpan("type ")
      const body = tree.body
      const bodyPath = path.x("body")
      this.appendSymSpan(new TypeDefSpan(this.root, path.x("sym"), tree))
      if (body instanceof T.TAbsTree) {
        this.appendTAbs(bodyPath, body, 0)
      } else {
        this.appendKeySpan(" = ")
        this.appendTree(bodyPath, body)
      }

    } else if (tree instanceof T.CtorTree) {
      this.depth += 1
      this.appendSymSpan(new TypeDefSpan(this.root, path.x("sym"), tree))
      this.depth -= 1
      this.appendTree(path.x("prod"), tree.prod)
      this.appendAnnot(start, tree.displaySig)

    } else if (tree instanceof T.THoleTree) {
      this.appendSymSpan(new TypeExprSpan(this.root, path, tree, new S.TypeHoleSym()))

    } else if (tree instanceof T.TConstTree) {
      this.appendSymSpan(new TypeExprSpan(this.root, path, tree, new S.TypeConstSym(tree.cnst)))

    } else if (tree instanceof T.ArrowTree) {
      this.appendTree(path.x("from"), tree.from)
      this.appendKeySpan(" → ")
      this.appendTree(path.x("to"), tree.to)

    // } else if (tpe instanceof T.ScalarDef) {
    //   this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)

    } else if (tree instanceof T.TRefTree) {
      this.appendSymSpan(new TypeExprSpan(this.root, path, tree, tree.sym))

    } else if (tree instanceof T.TAbsTree) {
      this.appendTAbs(path, tree, 0)

    } else if (tree instanceof T.TAppTree) {
      this.appendTree(path.x("ctor"), tree.ctor)
      // this.appendSepSpan("[")
      this.appendSepSpan(" ")
      this.appendTree(path.x("arg"), tree.arg)
      // this.appendSepSpan("]")
      this.appendAnnot(start, tree.displaySig.kind)

    } else if (tree instanceof T.FieldTree) {
      this.appendSymSpan(new TermDefSpan(this.root, path.x("sym"), tree))
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
      this.appendSymSpan(new TermExprSpan(this.root, path, tree, new S.TermConstSym(tree.cnst)))

    } else if (tree instanceof T.RefTree) {
      this.appendSymSpan(new TermExprSpan(this.root, path, tree, tree.sym))

    } else if (tree instanceof T.HoleTree) {
      this.appendSymSpan(new TermExprSpan(this.root, path, tree, new S.TermHoleSym()))

    } else if (tree instanceof T.PatTree) {
      this.appendPatTree(path, tree)

    } else if (tree instanceof T.AppTree) {
      if (editing) this.appendPreExprSpan("(", path, tree)
      this.appendTree(path.x("fun"), tree.fun)
      this.appendSepSpan(" ")
      // const wantParens = (tree.fun.kind !== "inapp" && tree.arg.kind === "app")
      const wantParens = !editing && (tree.fun.kind === "inapp" || tree.arg.kind === "app")
      if (wantParens) this.appendSepSpan("(")
      this.appendTree(path.x("arg"), tree.arg)
      if (wantParens) this.appendSepSpan(")")
      this.appendAnnot(start, tree.displaySig)
      if (editing) this.appendSepSpan(")")

    } else if (tree instanceof T.InAppTree) {
      // if (editing) this.appendPreExprSpan("(", path, tree)
      const wantParens = !editing && (tree.arg instanceof T.AppTree && tree.arg.fun.kind === "inapp")
      if (wantParens) this.appendSepSpan("(")
      this.appendTree(path.x("arg"), tree.arg)
      if (wantParens) this.appendSepSpan(")")
      this.appendSepSpan(" ")
      this.appendTree(path.x("fun"), tree.fun)
      this.appendAnnot(start, tree.displaySig)
      // if (editing) this.appendSepSpan(")")

    } else if (tree instanceof T.LetTree) {
      this.appendKeySpan("let ")
      this.appendSymSpan(new TermDefSpan(this.root, path.x("sym"), tree))
      this.appendAnnType(path.x("type"), tree.type)
      this.appendKeySpan(" = ")
      this.appendTree(path.x("body"), tree.body)
      this.appendAnnot(start, tree.displaySig)
      this.newLine()
      this.appendKeySpan("in ")
      this.appendTree(path.x("expr"), tree.expr)

    } else if (tree instanceof T.LetFunTree) {
      this.appendKeySpan("fun ")
      this.appendSymSpan(new TermDefSpan(this.root, path.x("sym"), tree))
      this.appendSepSpan(" ")
      this.appendTree(path.x("body"), tree.body)
      this.appendAnnot(start, tree.displaySig)
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
      this.appendAnnot(start, tree.displaySig)

    } else if (tree instanceof T.MatchTree) {
      this.appendKeySpan("case ")
      this.appendTree(path.x("scrut"), tree.scrut)
      this.appendKeySpan(" of")
      this.appendAnnot(start, tree.displaySig)
      for (let ii = 0; ii < tree.cases.length; ii += 1) {
        this.newLine()
        this.appendSubTree(path.x(`${ii}`), tree.cases[ii])
      }

    } else if (tree instanceof T.CaseTree) {
      this.appendTree(path.x("pat"), tree.pat)
      this.appendKeySpan(" → ")
      this.appendTree(path.x("body"), tree.body)
      this.appendAnnot(start, tree.displaySig)

    } else if (tree instanceof T.PrimTree) {
      this.appendSpan(new M.TextSpan("<internal>"))

    } else {
      throw new Error(`Unexpected tree: ${tree}`)
    }

    this.depth -= 1
    return this
  }

  maybeAppendError (path :T.Path, tree :T.Tree) {
    // const treeErr = tree.check()
    const treeSig = tree.displaySig
    if (treeSig instanceof TP.Error) {
      this.appendSpan(new ErrorSpan(this.root, path, tree, treeSig))
    }
  }

  appendPatTree (path :T.Path, tree :T.PatTree) {
    if (tree instanceof T.PHoleTree) {
      this.appendSymSpan(new PatSpan(this.root, path, tree, new S.TermHoleSym()))

    } else if (tree instanceof T.PLitTree) {
      this.appendSymSpan(new PatSpan(this.root, path, tree, new S.TermConstSym(tree.cnst)))

    } else if (tree instanceof T.PBindTree) {
      this.appendSymSpan(new PatSpan(this.root, path.x("sym"), tree, tree.sym))

    } else if (tree instanceof T.PDtorTree) {
      this.appendSymSpan(new PatSpan(this.root, path, tree, tree.ctor))

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
    let sub = new Acc(this.mod, this.root, this.focus, this.opts).
      appendTree(path, tree).finalize()
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
      this.appendSymSpan(new TypeExprSpan(this.root, path, tree, new S.TypeHoleSym()))
    } // otherwise append nothing
  }

  // Coalescing abs:
  // - abs inspects body: if it's another abs, append the arg & 'lift' the body
  appendAbs (path :T.Path, tree :T.AbsTree, pos :number) {
    const bodyPath = path.x("body"), body = tree.body
    this.appendSymSpan(new TermDefSpan(this.root, path.x("sym"), tree))
    this.appendAnnType(path.x("type"), tree.type)
    if (body instanceof T.AbsTree) {
      this.appendKeySpan(" → ")
      this.appendAbs(bodyPath, body, pos+1)
    } else if (body instanceof T.AscTree) {
      this.appendKeySpan(" → ")
      this.appendTree(bodyPath.x("type"), body.type)
      this.appendKeySpan(" = ")
      this.maybeAppendError(bodyPath, body)
      this.newLine()
      this.appendSubTree(bodyPath.x("expr"), body.expr)
    } else {
      this.appendKeySpan(" = ")
      this.appendSubTree(bodyPath, body)
    }
  }

  appendAll (path :T.Path, tree :T.AllTree, pos :number) {
    const body = tree.body, bodyPath = path.x("body")
    this.appendKeySpan("∀")
    this.appendSymSpan(new TypeDefSpan(this.root, path.x("sym"), tree))
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
    const body = tree.body, bodyPath = path.x("body")
    this.appendKeySpan(" ∀")
    this.appendSymSpan(new TypeDefSpan(this.root, path.x("sym"), tree))
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
  appendSymSpan (span :SymRuleSpan) {
    const start = this.lineWidth, {path, sym, tree} = span
    this.appendSpan(span, path)
    this.appendAnnot(start, sym.kind === "term" ? tree.displaySig : tree.displaySig.kind)
  }

  appendPreExprSpan (text :string, path :T.Path, tree :T.Tree) {
    this.appendSpan(new PreExprSpan(this.root, path, tree, text))
  }
  // TODO: appendPostExprSpan

  appendAnnot (start :number, annot :TP.Type|TP.Kind) {
    if (!this.opts.showSigs) return
    let length = this.lineWidth - start
    this.sigAnnots.push(new SigAnnot(start, length, this.depth, `${annot}`))
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

function formatCompletion (name :string, kind :S.Kind, sig :TP.Type) :M.Line {
  const sigStyle = kind == "type" ? "kind" : "type"
  return new M.Line([new M.TextSpan(name, [kind]),
                     new M.TextSpan(` :`),
                     new M.TextSpan(sig.toString(), [sigStyle])], []) // TODO: better formating
}

class NameCompletion extends M.Completion {
  constructor (readonly text :string) { super() }
  get name () :string { return this.text }
  get type () :TP.Type { return TP.hole }
  display () :M.Line { return new M.Line([new M.TextSpan(this.text)], []) }
  equals (that :M.Completion) :boolean {
    return that instanceof NameCompletion && this.text === that.text
  }
  protected apply (te :T.TreeEditor) { return te.setName(this.text) }
}

abstract class SymbolCompletion extends M.Completion {
  constructor (readonly item :S.Symbol) { super() }
  get name () :Name { return this.item.name }
  get type () :TP.Type { return this.item.type(TP.hole, false) }
  display () :M.Line { return formatCompletion(this.item.name, this.item.kind, this.type) }
  equals (that :M.Completion) :boolean {
    return that instanceof SymbolCompletion && this.item === that.item
  }
}

class TermSymbolCompletion extends SymbolCompletion {
  protected apply (te :T.TreeEditor) { return te.setRef(this.item) }
}

// TODO: better formating
class TypeSymbolCompletion extends SymbolCompletion {
  protected apply (te :T.TreeEditor) { return te.setTRef(this.item) }
}

class TermHoleCompletion extends M.Completion {
  get name () :Name { return "?" }
  get type () :TP.Type { return TP.hole }
  display () { return new M.Line([new M.TextSpan("?")], []) }
  equals (that :M.Completion) { return that instanceof TermHoleCompletion }
  protected apply (te :T.TreeEditor) { return te.setHole() }
}
const termHoleCompletion = new TermHoleCompletion()

class TypeHoleCompletion extends M.Completion {
  get name () :Name { return "?" }
  get type () :TP.Type { return TP.hole }
  display () { return new M.Line([new M.TextSpan("?")], []) }
  equals (that :M.Completion) { return that instanceof TypeHoleCompletion }
  protected apply (te :T.TreeEditor) { return te.setTHole() }
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
  protected apply (te :T.TreeEditor) { return te.setLit(this.cnst) }
}
class PatConstCompletion extends ConstCompletion {
  protected apply (te :T.TreeEditor) { return te.setPLit(this.cnst) }
}

type KeyRule = {
  name :string
  chord :string
  pathPred :(root :T.DefTree, path :T.Path) => boolean
  apply :(root :T.DefTree, path :T.Path) => T.TreeEdit
  focusOp? :(root :T.DefTree, path :T.Path) => T.Path
}

const addArgType :KeyRule = {
  name: "addArgType",
  chord: "S-Semicolon",
  pathPred: (root, path) => (path.endsWith(root, "abs", "sym") ||
                             path.endsWith(root, "field", "sym")),
  apply: (root, path) => {
    const typePath = path.sib("type")
    return typePath.selected(root) instanceof T.EmptyTree ?
      typePath.edit(te => te.setTHole()) : T.noopEdit
  },
  focusOp: (root, path) => path.sib("type")
}

const isEmptyAbs = (tree :T.Tree) => tree instanceof T.AbsTree && tree.sym.name === ""
const isEmptyTAbs = (tree :T.Tree) => tree instanceof T.TAbsTree && tree.sym.name === ""
const isEmptyAll = (tree :T.Tree) => tree instanceof T.AllTree && tree.sym.name === ""

const addAbs :KeyRule = {
  name: "addAbs",
  chord: "Space",
  pathPred: (root, path) => (path.endsWith(root, "fundef", "sym") ||
                             path.endsWith(root, "letfun", "sym") ||
                             path.endsWith(root, "abs", "sym")),
  apply: (root, path) => {
    const body = path.sib("body")
    return isEmptyAbs(body.selectedTree(root)) ? T.noopEdit : body.edit(te => te.spliceAbs())
  },
  focusOp: (root, path) => path.sib("body").x("sym")
}

const addTAbs :KeyRule = {
  name: "addTAbs",
  chord: "Space",
  pathPred: (root, path) => (path.endsWith(root, "typedef", "sym") ||
                             path.endsWith(root, "tabs", "sym")),
  apply: (root, path) => {
    const body = path.sib("body")
    return isEmptyTAbs(body.selectedTree(root)) ? T.noopEdit : body.edit(te => te.spliceTAbs())
  },
  focusOp: (root, path) => path.sib("body").x("sym")
}

const addAll :KeyRule = {
  name: "addAll",
  chord: "S-Space",
  pathPred: (root, path) => (path.endsWith(root, "fundef", "sym") ||
                             path.endsWith(root, "letfun", "sym") ||
                             path.endsWith(root, "all", "sym")),
  apply: (root, path) => {
    const body = path.sib("body")
    return isEmptyAll(body.selectedTree(root)) ? T.noopEdit : body.edit(te => te.spliceAll())
  },
  focusOp: (root, path) => path.sib("body").x("sym")
}

const addInAppArg :KeyRule = {
  name: "addInAppArg",
  chord: "Space",
  // TODO: also check that selTree.sig.isArrow?
  pathPred: (root, path) => path.endsWith(root, "inapp", "fun"),
  apply: (root, path) => path.pop().edit(te => te.spliceApp()),
  focusOp: (root, path) => path.sib("arg")
}

const addInApp :KeyRule = {
  name: "addInApp",
  chord: "Space",
  pathPred: (root, path) => {
    const selTree = path.selectedTree(root)
    return selTree.kind === "lit" || (selTree.kind === "ref" && !selTree.displaySig.isArrow)
  },
  apply: (root, path) => path.edit(te => te.spliceInApp()),
  focusOp: (root, path) => path.x("fun")
}

const addFunArgs :KeyRule = {
  name: "addFunArgs",
  chord: "Space",
  pathPred: (root, path) => (!path.endsWith(root, "inapp", "fun") &&
                             // TODO: also check that fun does not already have needed app nodes
                             path.selectedTree(root).displaySig.isArrow),
  apply: (root, path) => {
    const selTree = path.selectedTree(root)
    // otherwise we're sticking a fun somewhere new, so add holes for all args
    const edits = []
    const args = selTree.displaySig.arity
    for (let ii = 0; ii < args; ii += 1) {
      edits.push(path.edit(te => te.spliceApp()))
    }
    return T.merge(...edits)
  },
  focusOp: (root, path) => path.x("arg")
}

const spaceAfterType :KeyRule = {
  name: "spaceAfterType",
  chord: "Space",
  pathPred: (root, path) => true,
  apply: (root, path) => {
    if (path.selectedTree(root).kind === "tref") { // TODO: and we're in an arg list
    }
    return T.noopEdit
  },
  focusOp: (root, path) => {
    console.log(`Space pressed after ${path.mkString(root)}: ${path.selectedTree(root)}`)
    if (path.selectedTree(root).kind === "tapp") {
      return path.x("arg")
    } else {
      return path.firstEditable(root)
    }
  }
}

const insertField :KeyRule = {
  name: "insertField",
  chord: "Space",
  pathPred: (root, path) => path.endsWith(root, "prod", "*", "field", "sym"),
  apply: (root, path) => {
    const fieldIdx = parseInt(path.id(path.length-2))
    return path.pop().pop().edit(te => te.insertField(fieldIdx))
  },
  focusOp: (root, path) => path
}

const eqToBody :KeyRule = {
  name: "eqToBody",
  chord: "Equal",
  pathPred: (root, path) => (path.endsWith(root, "typedef", "sym") ||
                             path.endsWith(root, "tabs", "sym") ||
                             path.endsWith(root, "fundef", "sym") ||
                             path.endsWith(root, "all", "sym") ||
                             path.endsWith(root, "abs", "sym")),
  apply: (root, path) => T.noopEdit,
  focusOp: (root, path) => findBody(path.selectionParent(root), path.pop()).firstEditable(root)
}

const deleteTypeTree :KeyRule = {
  name: "deleteTypeTree",
  chord: "Delete",
  pathPred: (root, path) => true,
  apply: (root, path) => path.edit(te => te.setTHole()),
  focusOp: (root, path) => path
}

const deleteExprTree :KeyRule = {
  name: "deleteExprTree",
  chord: "Delete",
  pathPred: (root, path) => true,
  apply: (root, path) => path.edit(te => te.setHole()),
  focusOp: (root, path) => path
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

class ErrorSpan extends M.TreeSpan {
  constructor (readonly root :T.DefTree, readonly path :T.Path,
               readonly tree :T.Tree, readonly sig :TP.Type) { super() }
  get styles () { return ["error"] }
  get sourceText () { return "!" }
  get tooltip () { return `${this.tree}: ${this.sig}` }
}

abstract class RuleSpan extends M.TreeSpan {

  abstract get keyRules () :KeyRule[]

  getMappings (editor :M.Editor) :K.Mapping[] {
    const {root, path, keyRules} = this
    let mappings :K.Mapping[] = []

    for (let rule of keyRules) if (rule.pathPred(root, path)) mappings.push({
      id: rule.name,
      descrip: rule.name,
      chord: rule.chord,
      isEdit: () => false,
      action: (kp :K.KeyPress) => {
        const edit = rule.apply(root, path)
        if (edit) {
          console.log(`FIRED ${rule.name} @ ${path}`)
          const focus = rule.focusOp && rule.focusOp(root, path)
          editor.applyAction({edit, focus})
        }
      }
    })

    return mappings
  }

  commitEdit (text :string, compM :M.Completion|void) :M.EditAction|void {
    const comp = compM || this.createDefaultComp(text)
    console.log(`Commiting edit: ${comp}`)
    return comp.act(this.path)
  }

  protected abstract createDefaultComp (text :string) :M.Completion
}

abstract class SymRuleSpan extends RuleSpan {
  abstract get sym () :S.Symbol
  get styles () { return [symStyle(this.sym)] }
  get sourceText () { return this.sym.name }
  get displayText () { return this.sym.displayName || "?" }
}

const typeKeyRules :KeyRule[] = [
  addAll,
  addAbs,
  addTAbs,
  addArgType,
  eqToBody,
]

class TypeDefSpan extends SymRuleSpan {
  constructor (readonly root :T.DefTree, readonly path :T.Path,
               readonly tree :T.DefTree) { super() }

  get sym () :S.Symbol { return this.tree.sym }
  get keyRules () { return typeKeyRules }
  get editPlaceHolder () { return "<name>" }

  get tooltip () :string {
    const symType = this.tree.displaySig
    return this.sym.kind === "type" ? symType.kind.toString() : symType.toString()
  }

  getCompletions (text :string) :M.Completion[] { return [] } // TODO
  protected createDefaultComp (text :string) { return new NameCompletion(text) }
}

const termDefKeyRules :KeyRule[] = [
  addAll,
  addAbs,
  addTAbs,
  addArgType,
  insertField,
  eqToBody,
]

class TermDefSpan extends SymRuleSpan {
  constructor (readonly root :T.DefTree, readonly path :T.Path,
               readonly tree :T.DefTree) { super() }

  get sym () :S.Symbol { return this.tree.sym }
  get keyRules () { return termDefKeyRules }
  get editPlaceHolder () { return "<name>" }

  getCompletions (text :string) :M.Completion[] { return [] } // TODO
  protected createDefaultComp (text :string) { return new NameCompletion(text) }
}

const typeExprKeyRules :KeyRule[] = [
  deleteTypeTree,
  spaceAfterType
]

class TypeExprSpan extends SymRuleSpan {
  constructor (readonly root :T.DefTree, readonly path :T.Path,
               readonly tree :T.Tree, readonly sym :S.Symbol) { super() }

  get keyRules () { return typeExprKeyRules }
  get editPlaceHolder () { return "<type>" }

  get tooltip () :string {
    const symType = this.tree.displaySig
    return this.sym.kind === "type" ? symType.kind.toString() : symType.toString()
  }

  getCompletions (text :string) :M.Completion[] {
    return this.scope.getCompletions(sym => sym.kind === "type", text).
      map(sym => new TypeSymbolCompletion(sym))
  }
  protected createDefaultComp (text :string) {
    if (text === "") return typeHoleCompletion
    const {root, path} = this
    return new TypeSymbolCompletion(path.selectionParent(root).scope.lookupType(text))
  }

  commitEdit (text :string, compM :M.Completion|void) :M.EditAction|void {
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
    // }
    const {root, path} = this
    const comp = compM || (text === "" ? typeHoleCompletion : new TypeSymbolCompletion(
      path.selectionParent(root).scope.lookupType(text)))
    let compKind = comp.type.kind

    // if we're setting a non-constructor, then no funny business
    // TODO: do we eventually want to handle inapp ctors?
    if (!(compKind instanceof TP.KArrow)) return comp.act(path)

    // if we're replacing the ctor in an existing app, then don't fool with the args
    // (TODO: if it takes more args, we could insert holes? what about fewer? seems fiddly)
    if (!path.selectsHole(root)) return comp.act(path)

    // otherwise we're sticking a type ctor somewhere new, so add holes for all args
    const edits = [comp.edit(path)]
    while (compKind instanceof TP.KArrow) {
      edits.push(path.edit(te => te.spliceTApp()))
      compKind = compKind.res
    }
    return {edit: T.merge(...edits), focus: path.x("arg")}
  }
}

class TmplCompletion extends M.Completion {
  get name () :Name { return this.disp } // TODO: this is a terrible lie...
  get type () :TP.Type { return  TP.hole }
  get isTemplate () :boolean { return true }
  constructor (readonly disp :string, readonly editfn :T.EditFn,
               readonly psuff :string[]) { super() }
  display () { return new M.Line([new M.TextSpan(this.disp)], []) }
  equals (that :M.Completion) :boolean {
    return that instanceof TmplCompletion && this.disp === that.disp }
  focus (path :T.Path) :T.Path { return path.concat(this.psuff) }
  protected apply (te :T.TreeEditor) { return this.editfn(te) }
}

const letCompletion = new TmplCompletion("let ? = ? in ?", te => te.setLet(""), ["sym"])
const funCompletion = new TmplCompletion("fun ? ? = ? in ?", te => te.setLetFun(""), ["sym"])
const caseCompletion = new TmplCompletion("case ? of ?", te => te.setMatch(), ["scrut"])
const ifCompletion = new TmplCompletion("if ? then ? else ?", te => te.setIf(), ["test"])

const preExprKeyRules :KeyRule[] = [
  deleteExprTree
]

class PreExprSpan extends RuleSpan {
  constructor (readonly root :T.DefTree, readonly path :T.Path,
               readonly tree :T.Tree, readonly sourceText :string) { super() }

  // get tooltip () :string {
  //   return this.sym.kind === "type" ? this.sym.type.kind.toString() : this.sym.type.toString()
  // }
  get styles () { return ["separator"] }

  get keyRules () :KeyRule[] { return preExprKeyRules }

  protected createDefaultComp (text :string) { return new NameCompletion(text) }

}

const termExprKeyRules :KeyRule[] = [
  deleteExprTree,
  addInApp,
  addInAppArg,
  addFunArgs
]

class TermExprSpan extends SymRuleSpan {
  constructor (readonly root :T.DefTree, readonly path :T.Path,
               readonly tree :T.Tree, readonly sym :S.Symbol) { super() }

  get keyRules () :KeyRule[] { return termExprKeyRules }
  get editPlaceHolder () { return "<expr>" }

  getCompletions (text :string) :M.Completion[] {
    // if the text is a literal, return a single completion which is that literal
    const asLit = C.parseLit(text)
    if (asLit !== undefined) return [new TermConstCompletion(asLit)]

    // otherwise find a symbol that completes the text & matches the desired type
    const sel = this.path.selected(this.root)
    const selType = sel instanceof T.Tree ? sel.prototype : TP.hole
    console.log(`getCompletions '${text}' :: ${selType} (via ${sel})`)
    const pred = (sym :S.Symbol) => {
      // if (sym.kind === "term") console.log(` check comp ${sym} :: ${sym.type} :: ${sym.type.constructor.name} :: ${sym.type.join(selType)}`)
      // TODO: we want terms that yield our target type
      return (sym.kind === "term") && !sym.type(TP.hole, false).join(selType).isError
    }
    const comps :M.Completion[] = this.scope.getCompletions(pred, text).
      map(sym => new TermSymbolCompletion(sym))

    // if the text starts with a tree-stub generator, include that option as well
    if ("let".startsWith(text)) comps.push(letCompletion)
    if ("fun".startsWith(text)) comps.push(funCompletion)
    if ("case".startsWith(text)) comps.push(caseCompletion)
    if ("if".startsWith(text)) comps.push(ifCompletion)
    return comps
  }

  // look up the text as a symbol (TODO...)
  protected createDefaultComp (text :string) {
    if (text === "") return termHoleCompletion
    const {root, path} = this
    return new TermSymbolCompletion(path.selectionParent(root).scope.lookupTerm(text))
  }
}

class PatBindCompletion extends M.Completion {
  constructor (readonly text :string, readonly type :TP.Type) { super() }
  get name () :Name { return this.text }
  display () :M.Line { return formatCompletion(this.text, "term", this.type) }
  equals (that :M.Completion) :boolean {
    return that instanceof PatBindCompletion && this.text === that.text
  }
  protected apply (te :T.TreeEditor) { return te.setPBind(this.text) }
}

const patKeyRules :KeyRule[] = [
  addArgType,
  addAll,
  addAbs,
  addTAbs,
  eqToBody, // TODO: change arrow to =, eqToBody moves to expr?
]

class PatDtorCompletion extends SymbolCompletion {
  edit (path :T.Path) {
    const edits = [path.edit(te => te.setPDtor(this.item))]
    let dtorType = this.item.type(TP.hole, false).skolemize(new Map())
    while (dtorType instanceof TP.Arrow) {
      edits.push(path.edit(te => te.splicePApp()))
      dtorType = dtorType.res
    }
    return T.merge(...edits)
  }
  protected apply (te :T.TreeEditor) :T.TreeEdit { throw new Error(`Unused`) }
}

class PatSpan extends SymRuleSpan {
  constructor (readonly root :T.DefTree, readonly path :T.Path,
               readonly tree :T.Tree, readonly sym :S.Symbol) { super() }

  get keyRules () { return patKeyRules }
  get editPlaceHolder () { return "<pat>" }

  getCompletions (text :string) :M.Completion[] {
    // if the text is a literal, return a single completion which is that literal
    const asLit = C.parseLit(text)
    if (asLit !== undefined) return [new PatConstCompletion(asLit)]

    console.log(`getCompletions for pat ${this.tree.prototype}`)

    // otherwise find a symbol that completes the text & matches the desired type
    const proto = this.tree.prototype
    const pred = (sym :S.Symbol) => (sym.kind === "term" && sym.flavor === "ctor" &&
                                     TP.patApplies(sym.type(TP.hole, false), proto))
    const comps :M.Completion[] = this.scope.getCompletions(pred, text).
      map(sym => new PatDtorCompletion(sym))

    if (comps.every(c => c.name !== text)) {
      comps.unshift(this.createDefaultComp(text))
    }
    return comps
  }

  protected createDefaultComp (text :string) {
    return new PatBindCompletion(text, this.tree.prototype) }
}
