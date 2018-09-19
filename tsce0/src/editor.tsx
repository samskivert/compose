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

export class DefStore implements K.Source {
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

  get mod () :MD.Module { return this.sym.mod }
  get selectedSpan () :M.Span|void { return this.elem.spanAt(this.curs.path) }

  // from K.Source
  readonly mappings :K.Mapping[] = [{
    descrip: "Move cursor left",
    chord: "ArrowLeft",
    action: kp => this.moveCursor(M.moveHoriz(M.HDir.Left)),
  }, {
    descrip: "Move cursor right",
    chord: "ArrowRight",
    action: kp => this.moveCursor(M.moveHoriz(M.HDir.Right)),
  }, {
    descrip: "Move cursor up",
    chord: "ArrowUp",
    action: kp => this.moveCursor(M.moveVert(M.VDir.Up)),
  }, {
    descrip: "Move cursor down",
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

  constructor (readonly sym :MD.DefSym, def :T.DefTree,
               readonly selStore :IComputedValue<DefStore|void>,
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

  moveCursor (mover :(elem :M.Elem, path :M.Path) => M.Path) {
    const oldPath = this.curs.path
    const newPath = mover(this.elem, oldPath)
    const selSpan = this.elem.spanAt(newPath)
    const selIsHole = selSpan ? selSpan.isHole : false
    // console.log(`moveCursor ${JSON.stringify(newPath)} (${selSpan} // ${selIsHole})`)
    this.curs = {path: newPath, editing: selIsHole}
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
    this.curs.editing = true
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
    const mode = (idx :number) => ((!prefixMatch || (idx != path.span)) ? Mode.Normal :
                                   (editing ? Mode.Edited : Mode.Selected))
    return spans.map((span, idx) => this.renderSpan(ppre, idx, mode(idx), span))
  }

  renderSpan (ppre :number[], idx :number, mode :Mode, span :M.Span) :JSX.Element {
    if (mode == Mode.Edited && span.isEditable) {
      console.log(`Edit span ${span} :: ${span.constructor.name}`)
      return <SpanEditor key={idx} defStore={this.props.store} defEditor={this}
                         store={new SpanStore(span)} span={span}
                         stopEditing={() => { this.props.store.curs.editing = false }}
                         moveCursor={(dir) => { this.props.store.moveCursor(M.moveHoriz(dir)) }} />
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

const blurPress :K.KeyPress = {
  chord: "Blur",
  key: "",
  isModifier: false,
  isModified: false,
  preventDefault: () => {}
}

const editCodes = new Set([
  "ArrowUp",
  "ArrowDown",
  "ArrowLeft",
  "ArrowRight",
  "Backspace",
  "Delete",
  "Space",
])

// TODO: this is messy, we're trying to figure out if a particular key press is editing the text in
// an HTML input field, or moving the cursor in the field; really we should just totally control the
// text editing process so we can know for sure rather than using these fragile heuristics
function isNameEdit (ev :K.KeyPress) :boolean {
  return !ev.isModified && (ev.key.length == 1 || editCodes.has(ev.chord))
}

@observer
export class SpanEditor  extends React.Component<{
  defStore :DefStore,
  defEditor :DefEditor,
  store :SpanStore,
  span :M.Span,
  stopEditing :() => void,
  moveCursor :(dir :M.HDir) => void
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
               onBlur={ev => this.handleKey(blurPress)}
               onKeyDown={ev => this.handleKey(K.mkKeyPress(ev.nativeEvent))} />
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

  handleKey (kp :K.KeyPress) {
    const store = this.props.store, chord = kp.chord
    // if this is just a modifier keypress, ignore it (don't set actionTaken)
    if (kp.isModifier) return
    // if key is up/down arrow, change selected completion
    else if (chord === "ArrowUp") store.selCompIdx = Math.max(store.selCompIdx-1, 0)
    else if (chord === "ArrowDown") store.selCompIdx =
      Math.min(store.selCompIdx+1, store.completions.length-1)
    else if (chord === "Escape") {
      kp.preventDefault()
      this.props.stopEditing()
    }
    else if ((chord == "Tab" || chord == "S-Tab") && !store.actionTaken) {
      kp.preventDefault()
      this.props.moveCursor(chord === "Tab" ? M.HDir.Right : M.HDir.Left)
    }
    else {
      const action = this.props.span.handleEdit(kp, store.text, store.selectedCompletion)
      if (action) {
        kp.preventDefault()
        const focus = this.props.defStore.applyAction(action)
        if ((chord == "Tab" || chord == "S-Tab") && !focus) this.props.moveCursor(
          (chord == "Tab") ? M.HDir.Right : M.HDir.Left)
      }
      // if the keypress is not editing the text inside the span, forward it back up to the def
      // editor to potentially be handled
      else if (!isNameEdit(kp)) this.props.defEditor.handleKey(kp)
      // otherwise just let the edit affect the text of the span (a name or a completion)
    }
    store.actionTaken = true
  }
}
