import * as React from 'react';
import { computed, observable, transaction, IComputedValue } from 'mobx'
import { observer } from 'mobx-react'

import * as F from './format'
import * as M from './markup'
import * as N from './names'
import * as T from './trees'

// Cursor and selection model

// | Models the editing cursor: indicates whether we're currently editing an editable leaf node
// | (literal or name) and the path to that node. If we're not editing, the path identifies the node
// | where editing will start if a "begin editing" operation is performed.
type Cursor = {path :M.Path, editing :boolean}

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
const enum Mode { Normal, Selected, Edited }

// -----------------
// Definition editor
// -----------------

type UndoEntry = {edit :T.TreeEdit, curs :Cursor}

function mkUndoEntry (edit :T.TreeEdit, {path, editing} :Cursor) :UndoEntry {
  return {edit, curs: {path, editing}}
}

export class DefStore {
  @observable def!  :T.DefTree
  @observable elem! :M.Elem
  @observable curs  :Cursor = {path: M.emptyPath, editing: false}
  @observable sel   :Selection|void = undefined
  @observable showTypes :boolean = false
  @observable showTree :boolean = false

  undoStack :UndoEntry[] = []
  redoStack :UndoEntry[] = []

  @computed get name () :N.Name { return this.def.sym.name }
  @computed get isActive () :boolean { return this.selStore.get() === this }

  get selectedSpan () :M.Span|void { return this.elem.spanAt(this.curs.path) }

  keyHandler :(ev :KeyboardEvent) => boolean = ev => true

  constructor (def :T.DefTree, readonly selStore :IComputedValue<DefStore|void>,
               readonly mkActive :() => void, editing :boolean = false) {
    this.setDef(def, def.firstEditable())
    this.curs.editing = editing
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
    let {elem, path} = F.format(def, focus, this.showTypes)
    transaction(() => {
      this.def = def
      this.elem = elem
      if (!focus) this.curs.editing = false
      else if (M.isEmptyPath(path)) {
        console.warn(`No path for focus: ${focus}`)
        console.warn(def.debugShow().join("\n"))
        this.curs.editing = false
      }
      else this.curs = {path, editing: true}
    })
  }

  toString () {
    return `${this.name}/${this.def}`
  }
}

function mkKeySig (ev :KeyboardEvent) :string {
  let sig = ev.code
  if (ev.metaKey) sig = 'M-' + sig
  if (ev.altKey) sig = 'A-' + sig
  if (ev.ctrlKey) sig = 'C-' + sig
  if (ev.shiftKey) sig = 'S-' + sig
  return sig
}

@observer
export class DefEditor extends React.Component<{store :DefStore}> {

  keymap = {
    "ArrowLeft":  () => this._moveCursor(M.moveHoriz(M.HDir.Left)),
    "ArrowRight": () => this._moveCursor(M.moveHoriz(M.HDir.Right)),
    "ArrowUp":    () => this._moveCursor(M.moveVert(M.VDir.Up)),
    "ArrowDown":  () => this._moveCursor(M.moveVert(M.VDir.Down)),
    "Tab":        () => this._moveCursor(M.moveHoriz(M.HDir.Right)),
    "S-Tab":      () => this._moveCursor(M.moveHoriz(M.HDir.Left)),
    "Enter":      () => this._startEdit(),
    "C-ArrowLeft":  () => this._insertHole(M.Dir.Left),
    "C-ArrowRight": () => this._insertHole(M.Dir.Right),
    "C-ArrowUp":    () => this._insertHole(M.Dir.Up),
    "C-ArrowDown":  () => this._insertHole(M.Dir.Down),
    "S-C-Minus":    () => this.props.store.undoAction(),
    "M-Z":          () => this.props.store.undoAction(),
    "C-Backslash":  () => this.props.store.redoAction(),
  }

  componentWillMount() {
    this.props.store.keyHandler = this.handleKey.bind(this)
  }

  handleKey (ev :KeyboardEvent) :boolean {
    const {curs} = this.props.store
    if (curs.editing) return false
    const keySig = mkKeySig(ev)
    const action = this.keymap[keySig]
    if (action) {
      ev.preventDefault()
      action()
      return true
    } else {
      console.log(`TODO: handleKey ${keySig} // ${ev.key}`)
      return false
    }
  }

  _moveCursor (mover :(elem :M.Elem, path :M.Path) => M.Path) {
    const store = this.props.store
    const oldPath = store.curs.path
    const newPath = mover(store.elem, oldPath)
    const selSpan = store.elem.spanAt(newPath)
    const selIsHole = selSpan ? selSpan.isHole : false
    // console.log(`moveCursor ${JSON.stringify(newPath)} (${selSpan} // ${selIsHole})`)
    store.curs = {path: newPath, editing: selIsHole}
  }

  _insertHole (dir :M.Dir) {
    const span = this.props.store.selectedSpan
    if (span) {
      const action = span.insertHole(dir)
      if (action) {
        this.props.store.applyAction(action)
      }
    }
    return true
  }

  _startEdit () {
    this.props.store.curs.editing = true
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
    const mode = (idx :number) => ((!prefixMatch || (idx != path.span)) ? Mode.Normal :
                                   (editing ? Mode.Edited : Mode.Selected))
    return spans.map((span, idx) => this.renderSpan(ppre, idx, mode(idx), span))
  }

  renderSpan (ppre :number[], idx :number, mode :Mode, span :M.Span) :JSX.Element {
    if (mode == Mode.Edited && span.isEditable) {
      console.log(`Edit span ${span} :: ${span.constructor.name}`)
      return <SpanEditor store={new SpanStore(span)} defStore={this.props.store}
                         key={idx} span={span}
                         stopEditing={() => { this.props.store.curs.editing = false }}
                         advanceCursor={() => { this._moveCursor(M.moveHoriz(M.HDir.Right)) }} />
    } else {
      const onPress = span.isEditable ? () => {
        this.props.store.curs = {path: M.mkPath(ppre, idx), editing: false}
        this.props.store.mkActive()
      } : undefined
      return spanSpan(span, idx, mode == Mode.Selected, this.props.store.isActive, onPress)
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
  const sstyles = selected ?
      span.styles.concat([active ? "selectedSpan" : "lowSelectedSpan"]) :
      span.styles
  return <span className={sstyles.join(" ")} title={span.tooltip || ""}
               onMouseDown={onPress}>{span.displayText}</span>
}

export class SpanStore {
  @observable text :string
  @observable completions :M.Completion[] = []
  @observable selCompIdx = 0
  // keeps track of whether some sort of action was taken by the user, indicating that they want us
  // to apply the currently selected completion
  actionTaken = false

  constructor (span :M.Span) {
    const text = this.text = span.sourceText
    this.completions = span.getCompletions(text)
  }

  get selectedCompletion () :M.Completion|void {
    return this.completions.length > 0 ? this.completions[this.selCompIdx] : undefined
  }
}

const blurEvent :M.KeyEvent = {
  key: "Blur",
  code: "0",
  ctrlKey :false,
  shiftKey: false,
  altKey: false,
  metaKey: false,
  preventDefault: () => {},
}

@observer
export class SpanEditor  extends React.Component<{
  store :SpanStore,
  defStore :DefStore,
  span :M.Span,
  stopEditing :() => void,
  advanceCursor :() => void
}> {

  render () {
    // console.log(`SpanEditor render ${this.props.span}`)
    const store = this.props.store
    const comps = store.completions.map((comp, ii) => {
      const line = comp.display()
      const isSelected = ii == store.selCompIdx
      const styles = isSelected ? ["selected"] : []
      return <div key={ii} className={styles.join(" ")}>{
        line.spans.map((ss, ii) => spanSpan(ss, ii))}</div>
    })
    return (
      <div className={"spanEditor"}>
        <input type="text" autoFocus={true}
               placeholder={this.props.span.editPlaceHolder}
               value={store.text}
               onChange={this.onChange.bind(this)}
               onBlur={ev => this.handleKey(blurEvent)}
               onKeyDown={ev => this.handleKey(ev.nativeEvent)} />
        {(comps.length > 0) && <div className={"completions"}>{comps}</div>}
      </div>
    )
  }

  onChange (ev :React.FormEvent<HTMLInputElement>) {
    const text = ev.currentTarget.value
    const store = this.props.store
    transaction(() => {
      store.text = text
      const oldComp = store.selectedCompletion
      const comps = this.props.span.getCompletions(text)
      store.completions = comps
      if (oldComp) {
        const oldIdx = comps.findIndex(comp => comp.equals(oldComp))
        if (oldIdx >= 0) store.selCompIdx = oldIdx
        else store.selCompIdx = 0
      } else store.selCompIdx = 0
    })
  }

  handleKey (keyEv :M.KeyEvent) {
    const store = this.props.store, key = keyEv.key
    // if key is up/down arrow, change selected completion
    if (key === "ArrowUp") {
      store.selCompIdx = Math.max(store.selCompIdx-1, 0)
    } else if (key === "ArrowDown") {
      store.selCompIdx = Math.min(store.selCompIdx+1, store.completions.length-1)
    } else if (key === "Escape") {
      keyEv.preventDefault()
      this.props.stopEditing()
    } else if (key == "Tab" && !store.actionTaken) {
      keyEv.preventDefault()
      this.props.advanceCursor()
    } else {
      const action = this.props.span.handleKey(keyEv, store.text, store.selectedCompletion)
      if (action) {
        keyEv.preventDefault()
        const focus = this.props.defStore.applyAction(action)
        // TODO: "advance" backwards on S-Tab?
        if (key == "Tab" && !focus) this.props.advanceCursor()
      } // otherwise just let the key be added to the text
    }
    store.actionTaken = true
  }
}
