import * as React from "react";
import { computed, observable, transaction, IComputedValue } from "mobx"
import { observer } from "mobx-react"

import * as F from "./format"
import * as M from "./markup"
import * as MD from "./module"
import * as N from "./names"
import * as T from "./trees"
import * as K from "./keymap"

// Cursor and selection model

// | Models the editing cursor: indicates whether we're currently editing an editable leaf node
// | (literal or name) and the path to that node. If we're not editing, the path identifies the node
// | where editing will start if a "begin editing" operation is performed.
type Cursor = {path :M.Path, editing :boolean, offset :number}

function navCursor (path :M.Path) {
  return {path, editing: false, offset: 0}
}
function editCursor (path :M.Path, offset :number) {
  return {path, editing: true, offset}
}

// | Models the editing selection: a range of nodes that are selected (and visually highlighted) so
// | that they may act as the target of an editing operation (like cut, extract into binding, etc.).
// |
// | The selection defaults to the AST node identified by the cursor but can be expanded to
// | encompass more of the AST (generally by moving the selection 'up' the AST) or less (by moving
// | the selection 'down' the AST toward the cursor).
// |
// | The cursor must always be inside the selection. When the cursor is moved, the selection is
// | reset such that it starts and ends at the cursor's node.
type Selection = {start :T.Path, end :T.Path}

// | A span is either normal, selected or being edited.
const enum SpanMode { Normal, Selected, Edited }

// -----------------
// Definition editor
// -----------------

type UndoEntry = {edit :T.TreeEdit, curs :Cursor}

function mkUndoEntry (edit :T.TreeEdit, {path, editing, offset} :Cursor) :UndoEntry {
  return {edit, curs: {path, editing, offset}}
}

export class DefStore implements K.Source {
  @observable def!  :T.DefTree
  @observable elem! :M.Elem
  @observable curs  :Cursor = navCursor(M.emptyPath)
  @observable sel   :Selection|void = undefined
  @observable showTypes :boolean = false
  @observable showTree :boolean = false

  undoStack :UndoEntry[] = []
  redoStack :UndoEntry[] = []

  @computed get name () :N.Name { return this.def.sym.name }
  @computed get isActive () :boolean { return this.selStore.get() === this }

  get mod () :MD.Module { return this.sym.mod }
  get selectedSpan () :M.Span|void { return this.elem.spanAt(this.curs.path) }

  // from K.Source
  readonly mappings :K.Mapping[] = [{
    descrip: "Move tree cursor left",
    chord: "ArrowLeft",
    action: kp => this.moveCursor(M.moveHoriz(M.HDir.Left)),
  }, {
    descrip: "Move tree cursor right",
    chord: "ArrowRight",
    action: kp => this.moveCursor(M.moveHoriz(M.HDir.Right)),
  }, {
    descrip: "Move tree cursor up",
    chord: "ArrowUp",
    action: kp => this.moveCursor(M.moveVert(M.VDir.Up)),
  }, {
    descrip: "Move tree cursor down",
    chord: "ArrowDown",
    action: kp => this.moveCursor(M.moveVert(M.VDir.Down)),
  }, {
    descrip: "Move to next element",
    chord: "Tab",
    action: kp => this.moveCursor(M.moveHoriz(M.HDir.Right)),
  }, {
    descrip: "Move to previous element",
    chord: "S-Tab",
    action: kp => this.moveCursor(M.moveHoriz(M.HDir.Left)),
  }, {
    descrip: "Edit current element",
    chord: "Enter",
    action: kp => this.startEdit()
  }, {
    descrip: "Insert hole to left",
    chord: "C-ArrowLeft",
    action: kp => this.insertHole(M.Dir.Left),
  }, {
    descrip: "Insert hole to right",
    chord: "C-ArrowRight",
    action: kp => this.insertHole(M.Dir.Right),
  }, {
    descrip: "Insert hole above",
    chord: "C-ArrowUp",
    action: kp => this.insertHole(M.Dir.Up),
  }, {
    descrip: "Insert hole below",
    chord: "C-ArrowDown",
    action: kp => this.insertHole(M.Dir.Down),
  }, {
    descrip: "Undo last edit",
    chord: "S-C-Minus",
    action: kp => this.undoAction(),
  }, {
    descrip: "Undo last edit",
    chord: "M-Z",
    action: kp => this.undoAction(),
  }, {
    descrip: "Redo last undone edit",
    chord: "C-Backslash",
    action: kp => this.redoAction(),
  }]

  // from K.Source
  handleKey (kp :K.KeyPress) :boolean {
    return false
  }

  constructor (readonly sym :MD.DefSym, def :T.DefTree,
               readonly keymap :K.Keymap,
               readonly selStore :IComputedValue<DefStore|void>,
               readonly mkActive :() => void) {
    this.setDef(def, def.firstEditable())
  }

  setShowTypes (showTypes :boolean) {
    if (this.showTypes != showTypes) {
      this.showTypes = showTypes
      this.setDef(this.def)
    }
  }

  applyAction (action :M.EditAction) :T.Path|void {
    let {edit, focus} = action
    const {root, undo} = edit(this.def)
    this.undoStack.push(mkUndoEntry(undo, this.curs))
    this.redoStack = []
    this.setDef(root, focus)
    return focus
  }
  undoAction () { this.xdoAction(this.undoStack, this.redoStack) }
  redoAction () { this.xdoAction(this.redoStack, this.undoStack) }

  moveCursor (mover :(elem :M.Elem, path :M.Path) => M.Path) {
    const oldPath = this.curs.path
    const newPath = mover(this.elem, oldPath)
    // console.log(`moveCursor ${JSON.stringify(newPath)}`)
    this.curs = navCursor(newPath)
  }

  insertHole (dir :M.Dir) {
    const span = this.selectedSpan
    if (span) {
      const action = span.insertHole(dir)
      if (action) {
        this.applyAction(action)
      }
    }
    return true
  }

  startEdit () {
    this.curs = editCursor(this.curs.path, 0)
  }
  stopEdit () {
    this.curs = navCursor(this.curs.path)
  }

  private xdoAction (popStack :UndoEntry[], pushStack :UndoEntry[]) {
    const entry = popStack.pop()
    if (entry) {
      const {edit, curs} = entry
      const {root, undo} = edit(this.def)
      pushStack.push(mkUndoEntry(undo, curs))
      this.setDef(root)
      this.curs = curs
    }
  }

  private setDef (def :T.DefTree, focus? :T.Path) {
    console.log(`Set def ${def}, focus: ${focus}`)
    let {elem, path} = F.format(this.mod, def, focus, this.showTypes)
    transaction(() => {
      this.def = def
      this.elem = elem
      if (!focus) this.curs = navCursor(this.curs.path)
      else if (M.isEmptyPath(path)) {
        console.warn(`No path for focus: ${focus}`)
        console.warn(def.debugShow().join("\n"))
        this.curs = navCursor(this.curs.path)
      }
      else this.curs = navCursor(path) // TODO: used to start editing...
    })
  }

  toString () {
    return `${this.name}/${this.def}`
  }
}

@observer
export class DefEditor extends React.Component<{store :DefStore}> {

  // componentWillMount () {
  //   this.props.store.keyHandler =
  //     // if we're editing, the span editor will handle the key
  //     ev => this.props.store.curs.editing ? false : this.handleKey(K.mkKeyPress(ev))
  // }
  // componentWillUnmount() {
  // }

  handleKey (kp :K.KeyPress) :boolean {
    // const action = this.keymap[kp.chord]
    // if (action) {
    //   kp.preventDefault()
    //   action()
    //   return true
    // }

    const span = this.props.store.selectedSpan
    if (span) {
      const action = span.handleKey(kp)
      if (action) {
        kp.preventDefault()
        const focus = this.props.store.applyAction(action)
        if (!focus && (kp.chord === "Tab" || kp.chord === "S-Tab")) this.props.store.moveCursor(
          M.moveHoriz(kp.chord === "Tab" ? M.HDir.Right : M.HDir.Left))
      }
    }

    console.log(`TODO: handleKey ${kp.chord} // ${kp.key}`)
    return false
  }

  render () {
    const {store} = this.props
    const {curs, elem} = store
    const cname = store.isActive ? "editor selectedEditor" : "editor"
    return (<div className={cname}>
              <div className="sideToggle">
                tree <input type="checkbox" checked={store.showTree}
                            onChange={ev => store.showTree = ev.target.checked}/>
              </div>
              <div className="sideToggle">
                Show: types <input type="checkbox" checked={store.showTypes}
                                   onChange={ev => store.setShowTypes(ev.target.checked)}/> &nbsp;
              </div>
              {this.renderElem([], 0, curs, elem)}
              {store.showTree ? <pre className="debugTree">{store.def.debugShow().join("\n")}</pre> : undefined}
            </div>)
  }

  renderElems (ppre :number[], curs :Cursor, elems :M.Elem[]) :JSX.Element[] {
    return elems.map((elem, idx) => this.renderElem(ppre.concat([idx]), idx, curs, elem))
  }

  renderElem (ppre :number[], idx :number, curs :Cursor, elem :M.Elem) :JSX.Element {
    if (elem instanceof M.Block) {
      return (<div key={idx} className="block">{this.renderElems(ppre, curs, elem.elems)}</div>)
    } else if (elem instanceof M.Para) {
      return (<div key={idx} className="docs">{this.renderSpans(ppre, curs, elem.spans)}</div>)
    } else if (elem instanceof M.Line) {
      if (elem.annots.length > 0) {
        return (<div key={idx}>
                  <div key="spans">{this.renderSpans(ppre, curs, elem.spans)}</div>
                  {elem.annots.map(renderAnnots)}
                </div>)
      } else return (<div key={idx}>{this.renderSpans(ppre, curs, elem.spans)}</div>)
    } else {
      return (<div key={idx}>Unknown elem: {elem}</div>)
    }
  }

  renderSpans (ppre :number[], {path, editing} :Cursor, spans :M.Span[]) :JSX.Element[] {
    const prefixMatch = M.prefixMatch(ppre, path)
    const spanMode = (idx :number) =>
      ((!prefixMatch || (idx != path.span)) ? SpanMode.Normal :
       (editing ? SpanMode.Edited : SpanMode.Selected))
    return spans.map((span, idx) => this.renderSpan(ppre, idx, spanMode(idx), span))
  }

  renderSpan (ppre :number[], idx :number, mode :SpanMode, span :M.Span) :JSX.Element {
    const active = this.props.store.isActive
    if (active && mode == SpanMode.Edited && span.isEditable) {
      console.log(`Edit span ${span} :: ${span.constructor.name}`)
      return <SpanEditor key={idx} store={new SpanStore(this.props.store, span)} span={span}
                         stopEditing={() => this.props.store.stopEdit()}
                         moveCursor={dir => this.props.store.moveCursor(M.moveHoriz(dir))} />
    } else {
      const onPress = span.isEditable ? () => {
        this.props.store.curs = navCursor(M.mkPath(ppre, idx))
        this.props.store.mkActive()
      } : undefined
      return spanSpan(span, idx, mode == SpanMode.Selected, active, onPress)
    }
  }
}

function renderAnnot (annot :M.Annot, idx :number) {
  const cname = annot.styles.join(" ")
  return <span key={idx} title={annot.tooltip} className={cname}>{annot.text}</span>
}
function renderAnnots (annots :M.Annot[], idx :number) {
  return <div key={idx}>{annots.map(renderAnnot)}</div>
}

function spanSpan (span :M.Span, idx :number, selected :Boolean = false, active :Boolean = true,
                   onPress? :() => void) :JSX.Element {
  const selstyle = active ? "selectedSpan" : "lowSelectedSpan"
  const className = (selected ? span.styles.concat([selstyle]) : span.styles).join(" ")
  const title = span.tooltip || ""
  return <span className={className} title={title} onMouseDown={onPress}>{span.displayText}</span>
}

export class SpanStore {
  @observable text :string
  @observable completions :M.Completion[] = []
  @observable selCompIdx = 0
  // keeps track of whether some sort of action was taken by the user, indicating that they want us
  // to apply the currently selected completion
  actionTaken = false

  constructor (readonly defStore :DefStore, readonly span :M.Span) {
    const text = this.text = span.sourceText
    this.completions = span.getCompletions(text)
  }

  get selectedCompletion () :M.Completion|void {
    return this.completions.length > 0 ? this.completions[this.selCompIdx] : undefined
  }

  moveCompletion (delta :number) {
    // TODO: stop at ends rather than wrap?
    this.selCompIdx = (this.selCompIdx+delta) % this.completions.length;
  }

  moveCursor (delta :number) {
    this._setOffset(this.defStore.curs.offset+delta)
  }

  jumpCursor (start :boolean) {
    this.defStore.curs.offset = start ? 0 : this.text.length
  }

  insertChar (char :string) {
    const text = this.text
    const offset = this.defStore.curs.offset
    const ntext = text.substring(0, offset) + char + text.substring(offset)
    transaction(() => {
      this.text = ntext
      this.defStore.curs.offset += 1
      const oldComp = this.selectedCompletion
      const comps = this.span.getCompletions(ntext)
      this.completions = comps
      if (oldComp) {
        const oldIdx = comps.findIndex(comp => comp.equals(oldComp))
        if (oldIdx >= 0) this.selCompIdx = oldIdx
        else this.selCompIdx = 0
      } else this.selCompIdx = 0
    })
  }

  deleteChar (count :number) {
    const text = this.text
    const offset = this.defStore.curs.offset
    this.text = text.substring(0, offset) + text.substring(offset+count)
    if (offset > this.text.length) this._setOffset(this.text.length)
  }

  _setOffset (pos :number) {
    this.defStore.curs.offset = Math.min(Math.max(pos, 0), this.text.length)
  }
}

@observer
export class SpanEditor  extends React.Component<{
  store :SpanStore,
  span :M.Span,
  stopEditing :() => void,
  moveCursor :(dir :M.HDir) => void
}> implements K.Source {

  // from K.Source
  get name () :N.Name { return "Tree node" }

  // from K.Source
  readonly mappings :K.Mapping[] = [{
    descrip: "Move text cursor left",
    chord: "ArrowLeft",
    action: kp => this.props.store.moveCursor(-1),
  }, {
    descrip: "Move text cursor right",
    chord: "ArrowRight",
    action: kp => this.props.store.moveCursor(1),
  }, {
    descrip: "Choose previous completion",
    chord: "ArrowUp",
    action: kp => this.props.store.moveCompletion(-1),
  }, {
    descrip: "Choose next completion",
    chord: "ArrowDown",
    action: kp => this.props.store.moveCompletion(1),
  }, {
    descrip: "Jump to start of text",
    chord: "C-KeyA",
    action: kp => this.props.store.jumpCursor(true),
  }, {
    descrip: "Jump to end of text",
    chord: "C-KeyE",
    action: kp => this.props.store.jumpCursor(false),
  }, {
    descrip: "Delete char under the cursor",
    chord: "Delete",
    action: kp => this.props.store.deleteChar(1),
  }, {
    descrip: "Delete char before the cursor",
    chord: "Backspace",
    action: kp => {
      if (this.props.store.defStore.curs.offset > 0) {
        this.props.store.moveCursor(-1)
        this.props.store.deleteChar(1)
      }
    },
  }, {
    descrip: "Exit edit mode",
    chord: "Enter",
    action: kp => this.props.stopEditing(),
  }]

  // from K.Source
  handleKey (kp :K.KeyPress) :boolean {
    const store = this.props.store, chord = kp.chord
    // if this is just a modifier keypress, ignore it (don't set actionTaken)
    if (kp.isModifier) return false
    else if ((chord == "Tab" || chord == "S-Tab") && !store.actionTaken) {
      kp.preventDefault()
      this.props.moveCursor(chord === "Tab" ? M.HDir.Right : M.HDir.Left)
    }
    else {
      const action = this.props.span.handleEdit(kp, store.text, store.selectedCompletion)
      if (action) {
        kp.preventDefault()
        const focus = this.props.store.defStore.applyAction(action)
        if ((chord == "Tab" || chord == "S-Tab") && !focus) this.props.moveCursor(
          (chord == "Tab") ? M.HDir.Right : M.HDir.Left)
      } else if (kp.isPrintable) {
        store.insertChar(kp.key)
      } else {
        console.log(`TODO: handle press ${JSON.stringify(kp)}`)
      }
    }
    store.actionTaken = true
    return true
  }

  componentWillMount () {
    this.props.store.defStore.keymap.addSource(this)
  }
  componentWillUnmount() {
    this.props.store.defStore.keymap.removeSource(this)
  }

  render () {
    // console.log(`SpanEditor render ${this.props.span}`)
    const {span, store} = this.props
    const comps = store.completions.map((comp, ii) => {
      const line = comp.display()
      const isSelected = ii == store.selCompIdx
      const styles = isSelected ? ["selected"] : []
      return <div key={ii} className={styles.join(" ")}>{
        line.spans.map((ss, ii) => spanSpan(ss, ii))}</div>
    })

    const lowName = span.styles.join(" ")
    const highName = span.styles.concat(["selectedSpan"]).join(" ")
    const title = span.tooltip || ""
    const offset = store.defStore.curs.offset
    // add a blank space at the end of the text for appending
    const text = store.text ? `${store.text} ` : "?"
    const pre = (offset > 0) ? text.substring(0, offset) : ""
    const post = (offset < text.length-1) ? text.substring(offset+1) : ""
    return (
      <div className={"spanEditor"}>
        {pre.length > 0 ? <span className={lowName} title={title}>{pre}</span> : undefined}
        <span className={highName} title={title}>{text[offset]}</span>
        {post.length > 0 ? <span className={lowName} title={title}>{post}</span> : undefined}
        {(comps.length > 0) && <div className={"completions"}>{comps}</div>}
      </div>
    )
  }
}
