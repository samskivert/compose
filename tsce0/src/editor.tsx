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

const nullCursor :Cursor = {path: M.emptyPath, editing: false}

function followCursor ({path, editing} :Cursor, idx :number) :Cursor {
  return M.popPath(nullCursor, p => ({path: p, editing}), idx, path)
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
const enum Mode { Normal, Selected, Edited }

// -----------------
// Definition editor
// -----------------

export class DefStore {
  @observable def!  :T.DefTree
  @observable elem! :M.Elem
  @observable curs  :Cursor = {path: M.emptyPath, editing: false}
  @observable sel   :Selection|void = undefined
  @observable showTypes :boolean = false
  @observable showTree :boolean = false

  @computed get name () :N.Name { return this.def.self.name }
  @computed get isActive () :boolean { return this.selStore.get() === this }

  keyHandler :(ev :KeyboardEvent) => boolean = ev => true

  constructor (def :T.DefTree, readonly selStore :IComputedValue<DefStore|void>,
               editing :boolean = false) {
    this.setDef(def, def.firstEditable())
    this.curs.editing = editing
  }

  setDef (def :T.DefTree, focus? :T.Path) {
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

  setShowTypes (showTypes :boolean) {
    if (this.showTypes != showTypes) {
      this.showTypes = showTypes
      this.setDef(this.def)
    }
  }

  toString () {
    return `${this.name}/${this.def}`
  }
}

@observer
export class DefEditor extends React.Component<{store :DefStore}> {

  componentWillMount() {
    this.props.store.keyHandler = this.handleKey.bind(this)
  }

  handleKey (ev :KeyboardEvent) :boolean {
    const {curs} = this.props.store
    if (curs.editing) return false

    switch (ev.code) {
    case "ArrowLeft":  this._moveCursor(M.moveHoriz(M.HDir.Left))  ; break
    case "ArrowRight": this._moveCursor(M.moveHoriz(M.HDir.Right)) ; break
    case "ArrowUp":    this._moveCursor(M.moveVert(M.VDir.Up))     ; break
    case "ArrowDown":  this._moveCursor(M.moveVert(M.VDir.Down))   ; break
    case "Enter":
      this._startEdit()
      break
    case "Tab":
      const dir = ev.shiftKey ? M.HDir.Left : M.HDir.Right
      this._moveCursor(M.moveHoriz(dir))
      ev.preventDefault()
      break
    default:
      console.log(`TODO: handleKey ${ev.code}`)
      break
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
              {this.renderElem(0, curs, elem)}
              {store.showTree ? <pre className="debugTree">{store.def.debugShow().join("\n")}</pre> : undefined}
            </div>)
  }

  renderElems (curs :Cursor, elems :M.Elem[]) :JSX.Element[] {
    return elems.map((elem, idx) => this.renderElem(idx, followCursor(curs, idx), elem))
  }

  renderElem (idx :number, curs :Cursor, elem :M.Elem) :JSX.Element {
    if (elem instanceof M.Block) {
      return (<div key={idx} className="block">{this.renderElems(curs, elem.elems)}</div>)
    } else if (elem instanceof M.Para) {
      return (<div key={idx} className="docs">{this.renderSpans(curs, elem.spans)}</div>)
    } else if (elem instanceof M.Line) {
      if (elem.annots.length > 0) {
        return (<div key={idx}>
                  <div key="spans">{this.renderSpans(curs, elem.spans)}</div>
                  {elem.annots.map(renderAnnots)}
                </div>)
      } else return (<div key={idx}>{this.renderSpans(curs, elem.spans)}</div>)
    } else {
      return (<div key={idx}>Unknown elem: {elem}</div>)
    }
  }

  renderSpans ({path, editing} :Cursor, spans :M.Span[]) :JSX.Element[] {
    const mode = (idx :number) => (!(M.isLeaf(path) && (idx == path.span)) ? Mode.Normal :
                                   (editing ? Mode.Edited : Mode.Selected))
    return spans.map((span, idx) => this.renderSpan(idx, mode(idx), span))
  }

  renderSpan (idx :number, mode :Mode, span :M.Span) :JSX.Element {
    if (mode == Mode.Edited && span.isEditable) {
      return <SpanEditor store={new SpanStore(span)} defStore={this.props.store}
                         key={idx} span={span}
                         stopEditing={() => { this.props.store.curs.editing = false }}
                         advanceCursor={() => { this._moveCursor(M.moveHoriz(M.HDir.Right)) }} />
    } else {
      return spanSpan(span, idx, mode == Mode.Selected, this.props.store.isActive)
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

function spanSpan (span :M.Span, idx :number,
                   selected :Boolean = false, active :Boolean = true) :JSX.Element {
  const sstyles = selected ?
    span.styles.concat([active ? "selectedSpan" : "lowSelectedSpan"]) :
    span.styles
  return <span className={sstyles.join(" ")} key={idx}>{span.displayText}</span>
}

export class SpanStore {
  @observable text :string
  @observable completions :M.Completion[] = []
  @observable selCompIdx = 0

  constructor (span :M.Span) {
    const text = this.text = span.sourceText
    this.completions = span.getCompletions(text)
  }

  get selectedCompletion () :M.Completion|void {
    return this.completions.length > 0 ? this.completions[this.selCompIdx] : undefined
  }
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
               onBlur={ev => this.handleKey("Blur")}
               onKeyDown={ev => this.handleKey(ev.key, ev)} />
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

  handleKey (key :string, ev? :React.KeyboardEvent<HTMLInputElement>) {
    const store = this.props.store
    // if key is up/down arrow, change selected completion
    if (key == "ArrowUp") {
      store.selCompIdx = Math.max(store.selCompIdx-1, 0)
    } else if (key == "ArrowDown") {
      store.selCompIdx = Math.min(store.selCompIdx+1, store.completions.length-1)
    } else {
      const mods = !ev ? {} :
        {shift: ev.shiftKey, meta: ev.metaKey, alt: ev.altKey, ctrl: ev.ctrlKey}
      const action = this.props.span.handleKey(store.text, store.selectedCompletion, key, mods)
      if (action === "extend") return
      ev && ev.preventDefault()
      if (action === "cancel") this.props.stopEditing()
      else {
        let {tree, focus} = action
        this.props.defStore.setDef(tree, focus)
        if (key == "Tab" && !focus) this.props.advanceCursor()
      }
    }
  }
}
