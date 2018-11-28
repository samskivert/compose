import * as K from "./keymap"
import * as S from "./symbols"
import * as T from "./trees"
import * as TP from "./types"
import { Name } from "./names"

// ------------
// Markup model
// ------------

// The AST is transformed into a markup model which is used to display the code in a familiar
// "syntax highlighted, formatted text" format, but the text itself is not free-form editable.
// Instead, edits apply to the underlying AST and those changes propagate back out to the
// visualization.

/** Used to insert holes relative to a selected span. See `Span.insertHole`. */
export const enum  Dir { Up = 0, Down = 1, Left = 2, Right = 3 }

export interface Editor {
  readonly spanText :string
  readonly selectedComp :Completion|void
  textWithInsert (char :string) :string
  applyAction (action :EditAction) :T.Path|void
}

/** A sequence of characters that are displayed in a single style (typeface, weight, color, etc.).
  * Spans that represent components of an AST node will be editable, spans that display contextual
  * punctuation or keywords will not. */
export abstract class Span {

  /** A name that describes the underlying tree element (if any). */
  abstract get name () :string

  /** The source text of this span. If this is empty, `displayPlaceHolder` will be shown when
    * displaying this span in code and `editPlaceHolder` will be shown when editing the span. */
  abstract get sourceText () :string

  /** The styles to apply to the span when rendered. */
  abstract get styles () :string[]

  /** Whether or not this span is editable. */
  get isEditable () :boolean { return false }

  /** Whether or not this span displays a hole. */
  get isHole () :boolean { return false }

  /** The text to show in formatted code when `text` is empty. */
  get displayPlaceHolder () :string { return "?" }

  /** The placeholder text to show in the editor when `text` is empty. */
  get editPlaceHolder () :string { return "<?>" }

  /** The text to display for this span. */
  get displayText () :string { return this.sourceText || this.displayPlaceHolder }

  /** The tooltip text to display for this span. */
  get tooltip () :string|void { return undefined }

  /** Returns the completions to show given the current `text`. */
  getCompletions (text :string) :Completion[] { return [] }

  /** Inserts a hole relative to this span. If the span cannot insert a hole in the requested
    * direction, `undefined` is returned. */
  insertHole (dir :Dir) :EditAction|void { return undefined }

  /** Commits an edit to this span.
    * @param text the current text of the span.
    * @param comp the current completion (if one exists).
    * @return the action to take, if any.
    */
  commitEdit (text :string, comp :Completion|void) :EditAction|void { return undefined }

  /** Creates key mappings for this span given the supplied editor. */
  getMappings (editor :Editor) :K.Mapping[] { return [] }

  toString () :string { return this.displayText }
}

export abstract class EditableSpan extends Span {
  get isEditable () :boolean { return true }
}

/** A span containing uneditable `text` with `styles`. */
export class TextSpan extends Span {
  constructor (readonly sourceText :string, readonly styles :string[] = []) { super() }
  get name () :string { return this.sourceText }
}

/** A span representing an AST node. */
export abstract class TreeSpan extends EditableSpan {

  abstract get root () :T.DefTree
  abstract get path () :T.Path
  abstract get tree () :T.Tree

  get name () :string { return this.path.selectedId }
  get scope () :S.Scope { return this.tree.scope }

  get isHole () :boolean {
    const branch = this.path.selected(this.root)
    if (branch instanceof T.Tree && branch.isHole) return true
    if (branch instanceof S.Symbol && branch.name === "") return true
    return false
  }

  get tooltip () :string { return `${this.tree.displaySig} (< ${this.tree.prototype})` }

  insertHole (dir :Dir) :EditAction|void {
    const {root, path} = this
    switch (dir) {
    case Dir.Left:
      if (path.endsWith(root, "match", "scrut")) {
        // TODO: wrap the match in an app
        return undefined
      }
      if (path.endsWith(root, "app", "arg")) {
        // wrap the app fun in an app + hole
        const funPath = path.sib("fun")
        return {edit: funPath.edit(te => te.spliceApp()), focus: funPath.x("arg")}
      }
      break

    case Dir.Right:
      if (path.endsWith(root, "app", "arg")) {
        // wrap the app in an app + hole
        const appPath = path.pop()
        return {edit: appPath.edit(te => te.spliceApp()), focus: appPath.x("arg")}
      }
      break

    case Dir.Up:
      // if we're in a match case, add a case above
      const upCasePath = path.popTo(root, "case")
      if (upCasePath) {
        const caseTree = upCasePath.selected(root) as T.CaseTree
        const matchTree = upCasePath.selectionParent(root) as T.MatchTree
        const caseIdx = matchTree.cases.indexOf(caseTree)
        return insertCase(upCasePath.pop(), caseIdx)
      }
      break

    case Dir.Down:
      // if we're in the scrut of a match, add a case at position 0
      if (path.endsWith(root, "match", "scrut")) return insertCase(path.pop(), 0)
      // if we're in a match case, add a case below
      const dnCasePath = path.popTo(root, "case")
      if (dnCasePath) {
        const matchTree = dnCasePath.selectionParent(root) as T.MatchTree
        const caseIdx = matchTree.cases.indexOf(dnCasePath.selected(root) as T.CaseTree)
        return insertCase(dnCasePath.pop(), caseIdx+1)
      }
      break
    }
  }

  toString () { return `${this.displayText} @ ${this.path}` }
}

function insertCase (matchPath :T.Path, caseIdx :number) :EditAction {
  return {
    edit: matchPath.edit(te => te.insertCase(caseIdx)),
    focus: matchPath.x(`${caseIdx}`).x("pat")
  }
}

export abstract class Completion {
  /** The name of the completed element. */
  abstract readonly name :Name
  /** The type of the completed element. */
  abstract readonly type :TP.Type
  /** Returns true if this completion is a tree template with holes. */
  get isTemplate () :boolean { return false }
  /** The markup to display in the completion list. */
  abstract display () :Line
  /** Returns whether or not `this` is equal to `that`. */
  abstract equals (that :Completion) :boolean
  /** Edits the tree at `path` by applying this completion. */
  edit (path :T.Path) :T.TreeEdit { return path.edit(te => this.apply(te)) }
  /** Returns the focus to use after applying this completion's edit, if any. */
  focus (path :T.Path) :T.Path|void { return undefined }
  /** Applies this completion at `path` returning a full edit action. */
  act (path :T.Path) :EditAction {
    const focus = this.focus(path)
    return focus ? {edit: this.edit(path), focus} : {edit: this.edit(path)} // meh
  }
  toString () :string { return `${this.constructor.name}:${this.name}` }
  /** Applies this completion to the tree via `te`. */
  protected abstract apply (te :T.TreeEditor) :T.TreeEdit
}

// When a tree is formatted into a span, the editable spans know how to propagate edits back to the
// original AST. On every key press, the span receives the current edited text (prior to the key
// press being applied) and the key code for the pressed key. It then decides whether to extend the
// edit with the key, abort the edit (usually only when the escape key is pressed), or commit the
// edit, in which case it provides the replacement tree and an optional new focus, where the cursor
// will move.
//
// This allows editing to trigger different kinds of changes based on the key press that results in
// an edit being committed. For example, when editing the name introduced by a let binding, pressing
// `=` will jump immediately to the expression bound to that name, whereas pressing ` ` (space) will
// introduce a nested abstraction (effectively adding an argument to the function being defined by
// the let).

/** A change to apply in response to a keypress when editing a term: an edit to apply to the tree
  * and an optional new `focus`. */
export type EditAction = {edit :T.TreeEdit, focus? :T.Path}

// TODO: do we want a separate visualization model for docs?
// could allow the HTML layout engine to handle wrapping...

/** Defines a markup AST.
  *
  * Code is marked up as elements that are either `Block`s or `Line`s. A block aggregates lines and
  * is indented relative to its enclosing block, and a line aggregate spans. Note that wrapping of
  * spans is done during code formatting. We do not allow the HTML layout engine to wrap code.
  *
  * Documentation is marked up into elements that are either `Para`s or code `Block`s. A paragraph
  * is a collection of spans that _is_ allowed to be wrapped by the HTML layout engine. A code block
  * is just formatted (and editable) code that can be interspersed between paragraphs.
  *
  * TODO: do we need special support for inline code identifiers?
  */
export abstract class Elem {

  /** Extracts the sub-element identified by `idxs` (a path). */
  elemAt (idxs :number[]) :Elem { return this }

  /** Extracts the spans addressed by `idxs` from the tree rooted at `this` elem.
    * Yields `[]` if the path is invalid and does not address a `Line` or `Para`. */
  spansAt (idxs :number[]) :Span[] { return this.elemAt(idxs).spans }

  /** Extracts the span addressed by `path` from the tree rooted at `this` elem.
    * Yields `undefined` if the path is invalid. */
  spanAt (path :Path) :Span|void { return this.elemAt(path.idxs).spans[path.span] }

  protected get spans () :Span[] { return [] }

  debugShow () :string[] {
    const buf :string[] = []
    this._debugShow("", buf)
    return buf
  }
  _debugShow (indent :string, buf :string[]) {}
}

export class Block extends Elem {
  readonly kind = "block"
  constructor (readonly elems :Elem[]) { super() }

  elemAt (idxs :number[]) :Elem {
    if (idxs.length == 0) return this
    const child = this.elems[idxs[0]]
    return child.elemAt(idxs.slice(1))
  }

  _debugShow (indent :string, buf :string[]) {
    const nindent = indent + "  "
    this.elems.forEach(elem => elem._debugShow(nindent, buf))
  }
}

export class Para extends Elem {
  readonly kind = "para"
  get spans () :Span[] { return this._spans }
  constructor (readonly _spans :Span[]) { super() }

  _debugShow (indent :string, buf :string[]) {
    buf.push(indent + this.spans.map(span => span.displayText).join(""))
  }
}

export class Annot {
  constructor (readonly text :string,
               readonly tooltip :string,
               readonly styles :string[]) {}
}

export class Line extends Elem {
  readonly kind = "line"
  get spans () :Span[] { return this._spans }
  constructor (readonly _spans :Span[], readonly annots :Annot[][]) { super () }

  _debugShow (indent :string, buf :string[]) {
    buf.push(indent + this.spans.map(span => span.displayText).join(""))
  }
}

// ----------
// Path model
// ----------

// We use paths to keep track of the cursor when editing a marked up AST. Given a markup tree (Elem)
// and a path, we can move the cursor left, right, up, down, based on the visual structure of the
// markup.

/** Identifies a path from some root Elem to a Span. `idxs` indicates the indices of each nested
  * `Block` starting from the root of the markup tree. The final component of `idxs` identifies a
  * `Para` or `Line`. `span` is the index of the addressed span inside the addressed para or line.
  */
export type Path = {idxs :number[], span :number}

export const emptyPath = {idxs: [], span: -1}
export const mkPath = (idxs :number[], span :number) => ({idxs, span})
export const isLeaf = (path :Path) => path.idxs.length == 0
export const isEmptyPath = (path :Path) => path === emptyPath
export const prefixMatch = (ppre :number[], path :Path) :boolean => {
  const idxs = path.idxs
  if (ppre.length != idxs.length) return false
  for (let ii = 0; ii < ppre.length; ii += 1) if (ppre[ii] !== idxs[ii]) return false
  return true
}

/** Removes the top component of a path and applies `op` to the remainder. If the top of the path is
  * not equal to `idx`, `dflt` is returned instead. */
export function popPath<A> (dflt :A, op :(path :Path) => A, idx :number, path :Path) :A {
  let {idxs, span} = path
  if (idxs.length == 0 || idxs[0] != idx) return dflt
  return op({idxs: idxs.slice(1), span})
}

/** Used to "move" a path left or right. See `moveHoriz`. */
export const enum HDir { Left = -1, Right = 1 }

export const moveHoriz = (dir :HDir) => (relem :Elem, path :Path) :Path => {
  let {idxs, span} = path, delta :number = dir
  // if we're able to move left or right, then we're done
  let nspan = findEditableSpan(relem.spansAt(idxs), delta, span)
  if (nspan !== undefined) return {idxs, span: nspan}
  // otherwise we need to either move down a line (to first editable span)
  if (dir == HDir.Right) return moveVert(VDir.Down)(relem, path)
  // or move up a line (to last editable span)
  const vpath = advanceVert(VDir.Up, relem, path)
  const vspans = relem.spansAt(vpath)
  return {idxs: vpath, span: findEditableSpan(vspans, -1, vspans.length) || 0}
}

/** Used to "move" a path up or down. See `moveVert`. */
export const enum  VDir { Up = -1, Down = 1 }

export const moveVert = (dir :VDir) => (relem :Elem, path :Path) :Path => {
  const vpath = advanceVert(dir, relem, path)
  const vspans = relem.spansAt(vpath)
  return {idxs: vpath, span: findEditableSpan(vspans, 1, -1) || 0}
}

function findEditableSpan (spans :Span[], delta :number, idx :number) :number|undefined {
  let nidx = idx + delta
  while (nidx >= 0 && nidx < spans.length) {
    const span = spans[nidx]
    if (span.isEditable) return nidx
    nidx += delta
  }
  return undefined
}

function advanceVert (dir :VDir, relem :Elem, {idxs, span} :Path) :number[] {
  // tries to move forward in the current element, otherwise moves up
  // the path one component and then tries to move forward there
  function forward (idxs :number[]) :number[] {
    if (idxs.length == 0) return idxs
    const {init, last} = unsnoc(idxs) ; const ielem = relem.elemAt(init)
    if (ielem instanceof Block) {
      return (last < lastidx(ielem.elems)) ? init.concat([last+1]) : forward(init)
    }
    return idxs
  }
  // tries to move backward in the current element, otherwise moves up
  // the path one component and then tries to move backward there
  function backup (idxs :number[]) :number[] {
    if (idxs.length == 0) return idxs
    const {init, last} = unsnoc(idxs)
    return (last > 0) ? init.concat([last-1]) : backup(init)
  }
  switch (dir) {
  case VDir.Down:
    const fidxs = forward(idxs)        // move forward to the next valid path
    const felem = relem.elemAt(fidxs)  // obtain the element at that path position
    const dsuff = firstEditable(felem) // get the path from that element to the first line/para
    const dpath = fidxs.concat(dsuff)  // combine the path prefix and suffix into a full path
    return dpath
  case VDir.Up:
    const bidxs = backup(idxs)         // move backward to the next valid path
    const lelem = relem.elemAt(bidxs)  // obtain the element at that path position
    const usuff = lastEditable(lelem)  // get the path from that element to the first line/para
    const upath = bidxs.concat(usuff)  // combine the prefix and suffix into a full path
    return upath
  }
}

function lastidx<A> (elems :A[]) :number {
  return elems.length-1
}

function unsnoc<A> (elems :A[]) :{init :A[], last :A} {
  const len = elems.length
  return {init: elems.slice(0, len-1), last :elems[len-1]}
}

function findEditable (picker :(elems :Elem[]) => number, elem :Elem) :number[] {
  function loop (suff :number[], elem :Elem) :number[] {
    if (elem instanceof Block) {
      const idx = picker(elem.elems)
      return loop(suff.concat([idx]), elem.elems[idx])
    } else {
      return suff
    }
  }
  return loop([], elem)
}
const firstEditable = (elem :Elem) => findEditable(x => 0, elem)
const lastEditable = (elem :Elem) => findEditable(lastidx, elem)
