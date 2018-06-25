import * as T from './trees'

// ------------
// Markup model
// ------------

// The AST is transformed into a markup model which is used to display the code in a familiar
// "syntax highlighted, formatted text" format, but the text itself is not editable. Edits are made
// on the underlying AST and those changes propagate back out to the visualization.

/** A function used to update a tree based on the new text provided for a span. */
export type EditFn = (text :string, old :T.Tree) => {tree :T.Tree, focus? :T.Path}

/** A sequence of characters that are displayed in a single style (typeface, weight, color, etc.).
  * If the span represents a component of an AST node, its path will reflect the path to that node
  * (from the AST root used to generate the markup). Otherwise the path will be `Nil`. */
export type Span = {
  text :string,
  styles :string[],
  editor? :EditFn
  // TODO: other stuff like completion functions, indications on how many trailing spans
  // get slurped up if we edit this span, etc.
}

export function showSpan (span :Span) :string {
  return span.editor ? `[${span.text}]` : `<${span.text}>`
}

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

  /** Extracts the spans addressed by `idxs` from the tree rooted at `elem`.
    * Yields `[]` if the path is invalid and does not address a `Line` or `Para`. */
  spansAt (idxs :number[]) :Span[] { return this.elemAt(idxs).spans }

  debugShow () :string[] {
    const buf :string[] = []
    this._debugShow("", buf)
    return buf
  }

  protected get spans () :Span[] { return [] }
  _debugShow (indent :string, buf :string[]) {}
}

export class Block extends Elem {
  readonly kind = "block"
  constructor(readonly elems :Elem[]) { super() }

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
  constructor(readonly _spans :Span[]) { super() }

  _debugShow (indent :string, buf :string[]) {
    buf.push(indent + this.spans.map(span => span.text).join(""))
  }
}

export class Line extends Elem {
  readonly kind = "line"
  get spans () :Span[] { return this._spans }
  constructor(readonly _spans :Span[]) { super () }

  _debugShow (indent :string, buf :string[]) {
    buf.push(indent + this.spans.map(span => span.text).join(""))
  }
}

/** Creates a span containing `text` optional edit function and styles. */
export function span (text :string, editor? :EditFn, styles :string[] = []) :Span {
  return {text, styles, editor}
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
    if (span.editor) return nidx
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
