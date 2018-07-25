import * as M from "./markup"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"
import * as C from "./constants"

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
      this.appendTermDefSpan(path.x(("sym")), tree.scope, tree.sym, undefined, path.x("body"))
      this.appendSepSpan(" ")
      this.appendTree(path.x(("body")), tree.body)

    } else if (tree instanceof T.TypeDefTree) {
      if (this.depth == 0) this.appendKeySpan("type ")
      const body = tree.body
      const bodyPath = path.x(("body"))
      this.appendTypeDefSpan(path.x(("sym")), tree.scope, tree.sym, bodyPath)
      if (body instanceof T.TAbsTree) {
        this.appendTAbs(bodyPath, body, 0)
      } else {
        this.appendKeySpan(" = ")
        this.appendTree(bodyPath, body)
      }

    } else if (tree instanceof T.DefHoleTree) {
      this.appendSpan(new DefHoleSpan(this.root, path, tree.scope), path.x(("sym")))

    } else if (tree instanceof T.CtorTree) {
      this.depth += 1
      this.appendTypeDefSpan(path.x(("sym")), tree.scope, tree.sym, path.x(("prod")))
      this.depth -= 1
      this.appendTree(path.x(("prod")), tree.prod)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.EmptyTree) {
      // nothing (TODO?)

    } else if (tree instanceof T.THoleTree) {
      this.appendTypeExprSpan(path, tree.scope, "", tree.sig)

    } else if (tree instanceof T.TConstTree) {
      this.appendTypeExprSpan(path, tree.scope, tree.cnst.value, tree.sig)

    } else if (tree instanceof T.ArrowTree) {
      this.appendTree(path.x(("from")), tree.from)
      this.appendKeySpan(" → ")
      this.appendTree(path.x(("to")), tree.to)

    // } else if (tpe instanceof T.ScalarDef) {
    //   this.appendTypeExprSpan(path, `${tpe.tag}${tpe.size}`)

    } else if (tree instanceof T.TRefTree) {
      this.appendTypeExprSpan(path, tree.scope, tree.sym.name, tree.sig)

    } else if (tree instanceof T.TAbsTree) {
      this.appendTAbs(path, tree, 0)

    } else if (tree instanceof T.TAppTree) {
      this.appendTree(path.x(("ctor")), tree.ctor)
      // this.appendSepSpan("[")
      this.appendSepSpan(" ")
      this.appendTree(path.x(("arg")), tree.arg)
      // this.appendSepSpan("]")
      this.appendKindAnnot(start, tree.sig.kind)

    } else if (tree instanceof T.FieldTree) {
      this.appendTermDefSpan(path.x(("sym")), tree.scope, tree.sym)
      this.appendAnnType(path.x(("type")), tree.type)

    } else if (tree instanceof T.ProdTree) {
      for (let ii = 0; ii < tree.fields.length; ii += 1) {
        // TODO: newline separate if documented?
        // this.appendSepSpan(ii == 0 ? "(" : ",")
        this.appendSepSpan(" ")
        this.appendTree(path.x((`${ii}`)), tree.fields[ii])
      }
      // if (tree.branches.length > 0) this.appendSepSpan(")")

    } else if (tree instanceof T.SumTree) {
      for (let ii = 0; ii < tree.cases.length; ii += 1) {
        this.newLine()
        this.appendSubTree(path.x((`${ii}`)), tree.cases[ii])
      }

    } else if (tree instanceof T.LitTree) {
      this.appendExprSpan(path, tree.scope, tree.cnst.value, tree.sig, "constant")

    } else if (tree instanceof T.RefTree) {
      this.appendExprSpan(path, tree.scope, tree.sym.name, tree.sig, tree.sym.kind)

    } else if (tree instanceof T.HoleTree) {
      this.appendExprSpan(path, tree.scope, "", tree.sig, "term")

    } else if (tree instanceof T.PLitTree) {
      this.appendExprSpan(path, tree.scope, tree.cnst.value, tree.sig, "constant")

    } else if (tree instanceof T.PBindTree) {
      this.appendTermDefSpan(path.x(("sym")), tree.scope, tree.sym)

    } else if (tree instanceof T.PDtorTree) {
      this.appendExprSpan(path, tree.scope, tree.ctor.name, tree.sig, tree.ctor.kind)

    } else if (tree instanceof T.PAppTree) {
      this.appendTree(path.x(("fun")), tree.fun)
      this.appendSepSpan(" ")
      this.appendTree(path.x(("arg")), tree.arg)

    } else if (tree instanceof T.AppTree) {
      this.appendTree(path.x(("fun")), tree.fun)
      this.appendSepSpan(" ")
      const wantParens = (tree.fun.kind !== "inapp" && tree.arg.kind === "app")
      if (wantParens) this.appendSepSpan("(")
      this.appendTree(path.x(("arg")), tree.arg)
      if (wantParens) this.appendSepSpan(")")
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.InAppTree) {
      this.appendTree(path.x(("arg")), tree.arg)
      this.appendSepSpan(" ")
      this.appendTree(path.x(("fun")), tree.fun)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.LetTree) {
      this.appendKeySpan("let ")
      const typePath = path.x(("type"))
      const bodyPath = path.x(("body"))
      this.appendTermDefSpan(path.x(("sym")), tree.scope, tree.sym, typePath, bodyPath)
      this.appendAnnType(typePath, tree.type)
      this.appendKeySpan(" = ")
      this.appendTree(bodyPath, tree.body)
      this.appendTypeAnnot(start, tree.sig)
      this.newLine()
      this.appendKeySpan("in ")
      this.appendTree(path.x(("expr")), tree.expr)

    } else if (tree instanceof T.LetFunTree) {
      this.appendKeySpan("fun ")
      this.appendTermDefSpan(path.x(("sym")), tree.scope, tree.sym, undefined, path.x(("body")))
      this.appendSepSpan(" ")
      this.appendTree(path.x(("body")), tree.body)
      this.appendTypeAnnot(start, tree.sig)
      this.newLine()
      this.appendTree(path.x(("expr")), tree.expr)

    } else if (tree instanceof T.AscTree) {
      this.appendTree(path.x(("expr")), tree.expr)
      this.appendSepSpan(":")
      this.appendTree(path.x(("type")), tree.type)

    } else if (tree instanceof T.AllTree) {
      this.appendAll(path, tree, 0)

    } else if (tree instanceof T.AbsTree) {
      this.appendAbs(path, tree, 0)

    } else if (tree instanceof T.IfTree) {
      this.appendKeySpan("if ")
      this.appendTree(path.x(("test")), tree.test)
      this.appendKeySpan(" then ")
      this.appendTree(path.x(("texp")), tree.texp)
      this.appendKeySpan(" else ")
      this.appendTree(path.x(("fexp")), tree.fexp)
      this.appendTypeAnnot(start, tree.sig)

    } else if (tree instanceof T.MatchTree) {
      this.appendKeySpan("case ")
      this.appendTree(path.x(("scrut")), tree.scrut)
      this.appendKeySpan(" of")
      this.appendTypeAnnot(start, tree.sig)
      for (let ii = 0; ii < tree.cases.length; ii += 1) {
        this.newLine()
        this.appendSubTree(path.x((`${ii}`)), tree.cases[ii])
      }

    } else if (tree instanceof T.CaseTree) {
      this.appendTree(path.x(("pat")), tree.pat)
      this.appendKeySpan(" → ")
      this.appendTree(path.x(("body")), tree.body)
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

  appendAnnType (path :T.Path, tree :T.Tree, termBodyPath? :T.Path) {
    if (tree.kind !== "empty") {
      // this.appendKeySpan(":")
      this.appendSepSpan(":")
      this.appendTree(path, tree)
    } else if (this.focus && path.equals(this.focus)) {
      this.appendSepSpan(":")
      this.appendTypeExprSpan(path, tree.scope, "", tree.sig, undefined, termBodyPath)
    } // otherwise append nothing
  }

  // Coalescing abs:
  // - abs inspects body: if it's another abs, append the arg & 'lift' the body
  appendAbs (path :T.Path, tree :T.AbsTree, pos :number) {
    const argPath  = path.x("sym")
    const typePath = path.x("type")
    const bodyPath = path.x("body"), body = tree.body
    this.appendTermDefSpan(argPath, tree.scope, tree.sym, typePath, bodyPath)
    this.appendAnnType(typePath, tree.type, bodyPath)
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
    this.appendTypeDefSpan(path.x("sym"), tree.scope, sym, bodyPath)
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
    this.appendTypeDefSpan(path.x("sym"), tree.scope, sym, bodyPath)
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
  appendTypeDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol, bodyPath? :T.Path) {
    let start = this.lineWidth
    this.appendSpan(new TypeDefSpan(path, scope, sym, bodyPath), path)
    this.appendKindAnnot(start, sym.type.kind)
  }
  appendTypeExprSpan (path :T.Path, scope :S.Scope, text :string, type :TP.Type,
                      typeBodyPath? :T.Path, termBodyPath? :T.Path) {
    let start = this.lineWidth
    this.appendSpan(new TypeExprSpan(path, scope, text, typeBodyPath, termBodyPath), path)
    this.appendKindAnnot(start, type.kind)
  }
  appendTermDefSpan (path :T.Path, scope :S.Scope, sym :S.Symbol,
                     typePath? :T.Path, bodyPath? :T.Path) {
    let start = this.lineWidth
    this.appendSpan(new TermDefSpan(path, scope, sym, typePath, bodyPath), path)
    this.appendTypeAnnot(start, sym.type)
  }
  appendExprSpan (path :T.Path, scope :S.Scope, name :string, type :TP.Type, style :string) {
    let start = this.lineWidth
    this.appendSpan(new TermExprSpan(path, scope, name, [style]), path)
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
  constructor(readonly path :T.Path, readonly scope :S.Scope) { super() }

  edit (path :T.Path, editfn :T.EditFn, ...fsuff :string[]) :M.EditAction {
    const tree = path.edit(editfn)
    return fsuff.length > 0 ? {tree, focus: path.concat(fsuff)} : {tree}
  }

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

  constructor(readonly root :T.DefTree, path :T.Path, scope :S.Scope) { super(path, scope) }

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
    if (text === "fun") {
      const funtree = mkFun(msym)
      return {tree: funtree, focus: funtree.mkPath("sym")}
    // } else if (text === "let") {
      // TODO
    } else return "extend"
  }
}

class TypeDefSpan extends TreeSpan {
  constructor (
    path :T.Path,
    scope :S.Scope,
    readonly sym :S.Symbol,
    readonly bodyPath? :T.Path
  ) { super(path, scope) }

  get sourceText () { return this.sym.name }
  get styles () { return ["type"] }
  get editPlaceHolder () { return "<name>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    const {path, bodyPath} = this
    console.log(`typeNameEdit ${key} @ ${text}`)
    const setName :T.EditFn = te => te.setName(text)
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      return {tree: this.path.edit(setName), focus: bodyPath}
    case ":":
      // TODO: should we just allow : in type names? could be problematic?
      return {tree: this.path.edit(setName)}
    case " ":
      if (bodyPath) {
        path.edit(setName)
        return this.edit(bodyPath, te => te.setTAbs("").adopt("body", te.currentTree), "sym")
      }
      else return this.edit(path, setName)
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  getCompletions (text :string) :M.Completion[] {
    return [] // TODO
  }
}

class TypeExprSpan extends TreeSpan {
  constructor (
    path :T.Path,
    scope :S.Scope,
    readonly sourceText :string,
    readonly typeBodyPath? :T.Path,
    readonly termBodyPath? :T.Path
  ) { super(path, scope) }

  get styles () { return ["type"] }
  get editPlaceHolder () { return "<type>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    const setRef = (te :T.TreeEditor) =>
      text == "" ? te.setTHole() : te.setTRef(te.tree.scope.lookupType(text))
    const {path, typeBodyPath, termBodyPath} = this
    switch (key) {
    case "Enter":
    case "Tab":
      return this.edit(path, setRef)
    case " ":
      if (typeBodyPath) {
        return this.edit(typeBodyPath, te => te.setTAbs("").adopt("body", te.currentTree))
      }
      // TODO: stop the instanity, refactor all this to use composable rules
      else if (termBodyPath) {
        let tree = path.edit(setRef)
        // if the body is an empty abs, we want to simply start editing it
        // if not, we want to insert an empty abs first (and then edit that)
        let child = termBodyPath.selected
        if (!(child instanceof T.AbsTree) || child.sym.name !== "") {
          tree = termBodyPath.edit(te => {
            const oldBody = te.currentTree
            return (mods.shift ? te.setTAbs("").adopt("body", oldBody) :
                    te.setAbs("").adopt("body", oldBody))
          })
        }
        return ({tree, focus: termBodyPath.x("sym")})
      }
      else this.edit(path, setRef)
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
    path :T.Path,
    scope :S.Scope,
    readonly sym :S.Symbol,
    readonly typePath? :T.Path,
    readonly bodyPath? :T.Path
  ) { super(path, scope) }

  get styles () { return [this.sym.kind] }
  get sourceText () { return this.sym.name }
  get editPlaceHolder () { return "<name>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    const {path, bodyPath, typePath} = this
    console.log(`termNameEdit ${key} : ${text} @ ${path}`)
    const setName = (te :T.TreeEditor) => te.setName(text)
    const commitEdit = (text :string, focus? :T.Path) :M.EditAction =>
      ({tree: this.path.edit(te => te.setName(text)), focus})
    switch (key) {
    case "Enter":
    case "Tab":
    case "=":
      console.log(`Commit term edit ${path} => ${path.selected}`)
      return commitEdit(text, bodyPath)
    case ":":
      if (!typePath) return commitEdit(text)
      return ({tree: path.edit(setName), focus: typePath})
    case " ":
      if (!bodyPath) return commitEdit(text)
      let tree = path.edit(setName)
      // if the body is an empty abs, we want to simply start editing it
      // if not, we want to insert an empty abs first (and then edit that)
      let child = bodyPath.selected
      if (!(child instanceof T.AbsTree) || child.sym.name !== "") {
        tree = bodyPath.edit(te => {
          const oldBody = te.currentTree
          return (mods.shift ? te.setTAbs("").adopt("body", oldBody) :
                  te.setAbs("").adopt("body", oldBody))
        })
      }
      return ({tree, focus: bodyPath.x("sym")})
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  getCompletions (text :string) :M.Completion[] {
    return [] // TODO
  }
}

class TermExprSpan extends TreeSpan {
  constructor (
    path :T.Path,
    scope :S.Scope,
    readonly sourceText :string,
    readonly styles :string[]
  ) { super(path, scope) }

  get editPlaceHolder () { return "<expr>" }

  handleKey (text :string, comp :M.Completion|void, key :string, mods :M.Modifiers) :M.EditAction {
    console.log(`termExprEdit ${key} @ ${text}`)
    switch (key) {
    case "Enter":
    case "Tab":
    case " ":
      return this.commitEdit(text, comp, key === " ")
    case "Escape":
      return "cancel"
    default:
      return "extend"
    }
  }

  commitEdit (text :string, comp :M.Completion|void, viaSpace :boolean) :M.EditAction {
    const path = this.path
    const setValue = (editfn :T.EditFn) :M.EditAction => {
      if (!viaSpace) return this.edit(path, editfn)
      // if a non-fun is entered and then space is pressed, we want to create an inapp with this
      // value as the arg, and move the focus to the fun
      else return this.edit(path, te => te.setInApp().editBranches({
        "arg": editfn,
        "fun": fun => fun.setHole() // TODO: expect argType -> ? type...
      }), "fun")
    }

    // if the text is a literal, complete that (TODO: this should be via the completion; when
    // generating a list of completions for text that matches a literal, we should make the first
    // (and only) completion a tree that displays the literal with its appropriate (expected?) type)
    const asLit = C.parseLit(text)
    if (asLit !== undefined) return setValue(te => te.setLit(asLit))

    // if the text is a keyword, create the appropriate AST subtree
    switch (text) {
    case "let":  return this.edit(path, te => te.setLetH(), "sym")
    case "case": return this.edit(path, te => te.setMatchH(), "scrut")
    case "if":   return this.edit(path, te => te.setIfH(), "test")
    case "":     return this.edit(path, te => te.setHole(TP.hole))
    }

    // TODO: if we didn't get a completion, do something...
    if (!(comp instanceof SymbolCompletion)) return setValue(
      te => te.setRef(te.tree.scope.lookupTerm(text)))

    let termType = comp.item.type
    let edit :T.EditFn = te => te.setRef(comp.item)

    // if we're setting a non-function, use our setValue helper
    if (!(termType instanceof TP.Arrow)) return setValue(edit)

    // if we're setting the fun of an inapp, add holes in for the remaining args
    const lastIdx = path.length-1
    if (path.kindAt(lastIdx-1) === "inapp" && path.id(lastIdx) === "fun") {
      // first insert the ref into the current span hole
      path.edit(edit)
      // now pop up a level and wrap this whole inapp in an app with a hole for an arg
      return this.edit(path.pop(), te => {
        const oldInApp = te.currentTree
        te.setApp()
          .editBranch("arg", arg => arg.setHole()) // TODO: provide expected type from inapp sig
          .adopt("fun", oldInApp)
      }, "arg")
    }
    // otherwise we're just setting a fun, so add holes for all args
    else while (termType instanceof TP.Arrow) {
      const {from, to} = termType
      const inner = edit
      edit = te => te.setApp().editBranches({"fun": inner, "arg": arg => arg.setHole(from)})
      termType = to
    }
    return this.edit(path, edit)
  }

  getCompletions (text :string) :M.Completion[] {
    const pred = (sym :S.Symbol) => sym.kind === "term" || sym.kind === "func"
    return this.scope.getCompletions(pred, text).map(sym => new SymbolCompletion(sym))
  }
}
