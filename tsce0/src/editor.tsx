import * as React from 'react';
import { observable, transaction } from 'mobx'
import { observer } from 'mobx-react'

import * as M from './markup'
import * as T from './trees'
import * as F from './format'

// Cursor and selection model

// | Models the editing cursor: indicates whether we're currently editing an editable leaf node
// | (literal or name) and the path to that node. If we're not editing, the path identifies the node
// | where editing will start if a "begin editing" operation is performed.
type Cursor = {path :M.Path, editing :boolean}

const nullCursor :Cursor = {path: M.emptyPath, editing: false}

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

const emptySelection = {start: [], end: []}

// | A span is either normal, selected or being edited.
const enum Mode { Normal, Selected, Edited }

// -----------------
// Definition editor
// -----------------

/** The editor state is the def being edited, the current cursor, and the current selection. */
type State = { def :T.Def, elem :M.Elem, curs :Cursor, sel :Selection }

export function mkState (def :T.Def) :State {
  const {elem, path} = F.formatDef(def, def.firstEditable())
  // M.mkPath(M.firstEditable(elem), 0)
  const curs = {path, editing: false}
  return {def, elem, curs, sel: emptySelection}
}

function followCursor ({path, editing} :Cursor, idx :number) :Cursor {
  return M.popPath(nullCursor, p => ({path: p, editing}), idx, path)
}

export class DefStore {
  @observable def  :T.Def
  @observable elem :M.Elem
  @observable curs :Cursor = {path: M.emptyPath, editing: false}
  @observable sel  :Selection = emptySelection

  constructor(def :T.Def) {
    this.setDef(def, def.firstEditable())
  }

  setDef (def :T.Def, focus? :T.Path) {
    let {elem, path} = F.formatDef(def, focus || T.emptyPath)
    transaction(() => {
      this.def = def
      this.elem = elem
      if (focus) {
        console.log(`New cursor, and editing: ${path}`)
        this.curs = {path, editing: true}
      } else {
        this.curs.editing = false
      }
    })
  }
}

@observer
export class DefEditor extends React.Component<{store :DefStore}> {

  // TEMP
  componentWillMount() {
    document.addEventListener("keydown", this.handleKey.bind(this), false)
  }
  componentWillUnmount() {
    document.removeEventListener("keydown", this.handleKey.bind(this), false);
  }
  // END TEMP

  handleKey (ev :KeyboardEvent) :boolean {
    if (this.props.store.curs.editing) return false
    switch (ev.code) {
    case "ArrowLeft":  this._moveCursor(M.moveHoriz(M.HDir.Left))  ; break
    case "ArrowRight": this._moveCursor(M.moveHoriz(M.HDir.Right)) ; break
    case "ArrowUp":    this._moveCursor(M.moveVert(M.VDir.Up))     ; break
    case "ArrowDown":  this._moveCursor(M.moveVert(M.VDir.Down))   ; break
    case "Enter":      this._startEdit()                           ; break
    case "Tab":
      const dir = ev.shiftKey ? M.HDir.Left : M.HDir.Right
      this._moveCursor(M.moveHoriz(dir))
      ev.preventDefault()
      break;
    default:           console.log(`TODO: handleKey ${ev.code}`)   ; break
    }
    return true
  }

  _moveCursor (mover :(elem :M.Elem, path :M.Path) => M.Path) {
    const oldPath = this.props.store.curs.path
    const newPath = mover(this.props.store.elem, oldPath)
    console.log(`moveCursor ${JSON.stringify(newPath)}`)
    this.props.store.curs = {path: newPath, editing: false}
  }

  _startEdit () {
    this.props.store.curs.editing = true
  }

  render () {
    const {curs, elem} = this.props.store
    console.log(`render ${JSON.stringify(curs)}`)
    return (<div className="topdef">{this.renderElem(curs, elem)}</div>)
  }

  renderElems (curs :Cursor, elems :M.Elem[]) :JSX.Element[] {
    return elems.map((elem, idx) => this.renderElem(followCursor(curs, idx), elem))
  }

  renderElem (curs :Cursor, elem :M.Elem) :JSX.Element {
    if (elem instanceof M.Block) {
      return (<div className="block">{this.renderElems(curs, elem.elems)}</div>)
    } else if (elem instanceof M.Para) {
      return (<div className="docs">{this.renderSpans(curs, elem.spans)}</div>)
    } else if (elem instanceof M.Line) {
      return (<div>{this.renderSpans(curs, elem.spans)}</div>)
    } else {
      return (<div>Unknown elem: {elem}</div>)
    }
  }

  renderSpans ({path, editing} :Cursor, spans :M.Span[]) :JSX.Element[] {
    const mode = (idx :number) => (!(M.isLeaf(path) && (idx == path.span)) ? Mode.Normal :
                                   (editing ? Mode.Edited : Mode.Selected))
    return spans.map((span, idx) => this.renderSpan(mode(idx), span))
  }

  renderSpan (mode :Mode, {text, styles, editor} :M.Span) :JSX.Element {
    if (mode == Mode.Edited && editor) {
      return <SpanEditor store={new SpanStore(text)} defStore={this.props.store} editor={editor}
                         stopEditing={() => { this.props.store.curs.editing = false }}
                         advanceCursor={() => { this._moveCursor(M.moveHoriz(M.HDir.Right)) }} />
    } else {
      const sstyles = (mode == Mode.Selected) ? styles.concat(["selected"]) : styles
      return <span className={sstyles.join(" ")}>{text}</span>
    }
  }
}

export class SpanStore {
  @observable text :string

  constructor (text :string) {
    this.text = text
  }
}

@observer
export class SpanEditor  extends React.Component<{
  store :SpanStore,
  defStore :DefStore,
  editor :M.Editor,
  stopEditing :() => void,
  advanceCursor :() => void
}> {

  render () { return (
    <input type="text" autoFocus={true} className={"spanEditor"}
           placeholder={this.props.editor.placeHolder}
           value={this.props.editor.startEdit(this.props.store.text)}
           onChange={this.onChange.bind(this)}
           onBlur={ev => this.handleKey("Blur")}
           onKeyDown={ev => this.handleKey(ev.key, ev)} />
  )}

  onChange (ev :React.FormEvent<HTMLInputElement>) {
    this.props.store.text = ev.currentTarget.value
  }

  handleKey (key :string, ev? :React.KeyboardEvent<HTMLInputElement>) {
    const action = this.props.editor.handleKey(this.props.store.text, key)
    if (action === "extend") return
    ev && ev.preventDefault()
    if (action === "cancel") this.props.stopEditing()
    else {
      let {tree, focus} = action
      this.props.defStore.setDef(tree as T.Def, focus)
      if (key == "Tab") this.props.advanceCursor()
    }
  }
}