import * as React from "react";
import { computed, observable, observe, transaction, IComputedValue } from "mobx"
import { observer } from "mobx-react"

import * as F from "./format"
import * as M from "./markup"
import * as MD from "./module"
import * as N from "./names"
import * as T from "./trees"
import * as K from "./keymap"

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

// -----------------
// Definition editor
// -----------------

type UndoEntry = {edit :T.TreeEdit, curs :M.Path}

function mkUndoEntry (edit :T.TreeEdit, curs :M.Path) :UndoEntry {
  return {edit, curs}
}

export class DefStore implements K.Source {
  @observable def!  :T.DefTree
  @observable elem! :M.Elem
  @observable showTypes :boolean = false
  @observable showTree  :boolean = false

  undoStack :UndoEntry[] = []
  redoStack :UndoEntry[] = []

  @observable curs :M.Path = M.emptyPath
  @observable sel  :Selection|void = undefined

  @computed get name () :N.Name { return this.def.sym.name }
  @computed get isActive () :boolean { return this.selStore.get() === this }

  get mod () :MD.Module { return this.sym.mod }

  get selectedSpan () :M.Span {
    const span = this.elem.spanAt(this.curs)
    if (span) return span
    throw new Error(`No selected span for ${this.def}: invalid cursor ${this.curs}`)
  }

  bind (keymap :K.Keymap) {
    keymap.addSource(this)
  }
  unbind (keymap :K.Keymap) {
    keymap.removeSource(this)
  }

  constructor (readonly sym :MD.DefSym, def :T.DefTree,
               readonly keymap :K.Keymap,
               readonly selStore :IComputedValue<DefStore|void>,
               readonly mkActive :() => void) {
    this.setDef(def, def.firstEditable())
    observe(this, "curs", change => {
      // when the cursor changes, always start spanText with the text for the new span; it may
      // subsequently get edited if we enter edit mode and start changing it
      this.spanText = this.selectedSpan.sourceText
    })
  }

  toString () {
    return `${this.name}/${this.def}`
  }

  //
  // K.Source functionality

  readonly mappings :K.Mapping[] = [{
    descrip: "Move cursor left",
    chord: "ArrowLeft",
    action: kp => {
      if (this.isEditing) this.moveTextCursor(-1)
      else this.moveCursor(M.moveHoriz(M.HDir.Left))
    },
  }, {
    descrip: "Move cursor right",
    chord: "ArrowRight",
    action: kp => {
      if (this.isEditing) this.moveTextCursor(1)
      else this.moveCursor(M.moveHoriz(M.HDir.Right))
    },
  }, {
    descrip: "Move cursor up",
    // descrip: "Choose previous completion",
    chord: "ArrowUp",
    action: kp => {
      if (this.isEditing) this.moveCompletion(-1)
      else this.moveCursor(M.moveVert(M.VDir.Up))
    },
  }, {
    descrip: "Move cursor down",
    // descrip: "Choose next completion",
    chord: "ArrowDown",
    action: kp => {
      if (this.isEditing) this.moveCompletion(1)
      else this.moveCursor(M.moveVert(M.VDir.Down))
    },
  }, {
    descrip: "Move to next element",
    chord: "Tab",
    action: kp => {
      if (this.isEditing) this.handleKey(kp)
      else this.moveCursor(M.moveHoriz(M.HDir.Right))
    },
  }, {
    descrip: "Move to previous element",
    chord: "S-Tab",
    action: kp => {
      if (this.isEditing) this.handleKey(kp)
      else this.moveCursor(M.moveHoriz(M.HDir.Left))
    },
  }, {
    descrip: "Jump to start of text",
    chord: "C-KeyA",
    action: kp => {
      if (this.isEditing) this.jumpTextCursor(true)
      // else: TODO: jump to start of line?
    },
  }, {
    descrip: "Jump to end of text",
    chord: "C-KeyE",
    action: kp => {
      if (this.isEditing) this.jumpTextCursor(false)
      // else: TODO: jump to start of line
    },
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
  }, {
    descrip: "Edit current element",
    chord: "Enter",
    action: kp => {
      if (this.isEditing) this.handleKey(kp) // TODO: right thing to do?
      else this.startEdit()
    }
  }, {
    descrip: "Cancel current edit",
    chord: "Escape",
    action: kp => this.stopEdit(),
  }, {
    descrip: "Delete char before the cursor",
    chord: "Backspace",
    action: kp => {
      if (!this.isEditing) {
        // TODO: delete all text, show hole?
      } else if (this.offset > 0) {
        this.moveTextCursor(-1)
        this.deleteChar(1)
      }
    },
  }, {
    descrip: "Delete char under the cursor",
    chord: "Delete",
    action: kp => {
      if (!this.isEditing) {} // TODO: delete all text, show hole?
      else this.deleteChar(1)
    },
  }]

  handleKey (kp :K.KeyPress) :boolean {
    console.log(`handleKey ${kp.chord} ${kp.isModifier}`)

    // if this is just a modifier keypress, ignore it (don't set actionTaken)
    if (kp.isModifier) return false

    const chord = kp.chord
    if ((chord == "Tab" || chord == "S-Tab") && !this.applyComp) {
      kp.preventDefault()
      this.moveCursor(M.moveHoriz(chord === "Tab" ? M.HDir.Right : M.HDir.Left))
      return true
    }

    const newText = kp.isPrintable ? this.textWithInsert(kp.key) : this.spanText
    const action = this.selectedSpan.handleEdit(kp, newText, this.selectedComp)
    if (action) {
      kp.preventDefault()
      const focus = this.applyAction(action)
      if ((chord == "Tab" || chord == "S-Tab") && !focus) this.moveCursor(
        M.moveHoriz(chord == "Tab" ? M.HDir.Right : M.HDir.Left))
    } else if (kp.isPrintable) {
      this.insertChar(kp.key)
    } else {
      console.log(`TODO: handle press ${JSON.stringify(kp)}`)
    }
    this.applyComp = true
    return true
  }

  //
  // Tree editing functionality

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

  selectSpan (path :M.Path, offset :number|void = undefined) {
    transaction(() => {
      this.curs = path
      this.offset = offset
    })
  }

  moveCursor (mover :(elem :M.Elem, path :M.Path) => M.Path) {
    const oldPath = this.curs
    const newPath = mover(this.elem, oldPath)
    const selSpan = this.elem.spanAt(newPath)
    // console.log(`moveCursor ${JSON.stringify(newPath)}`)
    this.selectSpan(newPath, selSpan && selSpan.isHole ? 0 : undefined)
  }

  insertHole (dir :M.Dir) {
    const span = this.selectedSpan
    const action = span.insertHole(dir)
    action && this.applyAction(action)
    return true
  }

  startEdit () {
    this.offset = this.selectedSpan.sourceText.length
  }
  stopEdit () {
    this.offset = undefined
    this.spanText = this.selectedSpan.sourceText
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
      if (focus) {
        if (M.isEmptyPath(path)) {
          console.warn(`No path for focus: ${focus}`)
          console.warn(def.debugShow().join("\n"))
        } else this.curs = path
      }
      this.offset = undefined
    })
  }

  //
  // Span text editing functionality

  /** When defined, indicates we're in text edit mode and represents the character offset into the
    * text of the selected span at which to place the text edit cursor. When undefined, indicates
    * we're in tree edit mode, such that we're only editing entire tree nodes. */
  @observable offset :number|void = undefined

  /** Whether or not we're in text edit mode. */
  get isEditing () :boolean { return this.offset !== undefined }

  @observable spanText :string = ""
  @observable selCompIdx = 0

  @computed get spanComps () :M.Completion[] {
    if (this.offset === undefined) return []
    const span = this.selectedSpan
    return span.getCompletions(this.spanText)
  }

  @computed get selectedComp () :M.Completion|void {
    return this.spanComps.length > 0 ? this.spanComps[this.selCompIdx] : undefined
  }

  // keeps track of whether some sort of action was taken by the user, and
  // thus whether they want us to apply the currently selected completion
  applyComp = false

  moveCompletion (delta :number) {
    // TODO: stop at ends rather than wrap?
    this.selCompIdx = (this.selCompIdx+delta) % this.spanComps.length;
  }

  moveTextCursor (delta :number) {
    const offset = this.offset || 0
    this._setOffset(offset+delta)
  }

  jumpTextCursor (start :boolean) {
    this.offset = start ? 0 : this.spanText.length
  }

  textWithInsert (char :string) :string {
    // if we're not currently editing the span text, replace it wholesale with this typed char;
    // otherwise insert the char at the edit position and return it
    const offset = this.offset
    if (offset === undefined) {
      return char
    } else {
      const text = this.spanText
      return text.substring(0, offset) + char + text.substring(offset)
    }
  }

  insertChar (char :string) {
    const oldComp = this.selectedComp
    // update the span text, which may trigger a completions update
    transaction(() => {
      this.spanText = this.textWithInsert(char)
      this.offset = (this.offset || 0) + 1
    })
    // keep the same completion selected, if possible
    const oldIdx = oldComp ? this.spanComps.findIndex(comp => comp.equals(oldComp)) : -1
    if (oldIdx < 0) this.selCompIdx = 0
    else this.selCompIdx = oldIdx
  }

  deleteChar (count :number) {
    const text = this.spanText
    const offset = this.offset || 0
    this.spanText = text.substring(0, offset) + text.substring(offset+count)
    if (offset > this.spanText.length) this._setOffset(this.spanText.length)
  }

  _setOffset (pos :number) {
    this.offset = Math.min(Math.max(pos, 0), this.spanText.length)
  }
}

@observer
export class DefEditor extends React.Component<{store :DefStore}> {

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

  renderElems (ppre :number[], curs :M.Path, elems :M.Elem[]) :JSX.Element[] {
    return elems.map((elem, idx) => this.renderElem(ppre.concat([idx]), idx, curs, elem))
  }

  renderElem (ppre :number[], idx :number, curs :M.Path, elem :M.Elem) :JSX.Element {
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

  renderSpans (ppre :number[], curs :M.Path, spans :M.Span[]) :JSX.Element[] {
    const prefixMatch = M.prefixMatch(ppre, curs)
    const isSelected = (idx :number) => prefixMatch && idx == curs.span
    return spans.map((span, idx) => this.renderSpan(ppre, idx, isSelected(idx), span))
  }

  renderSpan (ppre :number[], idx :number, selected :boolean, span :M.Span) :JSX.Element {
    const store = this.props.store, active = store.isActive
    if (selected && store.offset !== undefined && span.isEditable) {
      const comps = store.spanComps.map((comp, ii) => {
        const line = comp.display()
        const isSelected = ii == store.selCompIdx
        const styles = isSelected ? ["selected"] : []
        return <div key={ii} className={styles.join(" ")}>{
          line.spans.map((ss, ii) => spanSpan(ss, ii))}</div>
      })

      const lowName = span.styles.join(" ")
      const highName = span.styles.concat(["selectedSpan"]).join(" ")
      const title = span.tooltip || ""
      const offset = store.offset || 0
      // add a blank space at the end of the text for appending
      const text = store.spanText ? `${store.spanText} ` : "?"
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

    } else {
      const onPress = span.isEditable ? () => {
        store.selectSpan(M.mkPath(ppre, idx))
        store.mkActive()
      } : undefined
      return spanSpan(span, idx, selected, active, onPress)
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
