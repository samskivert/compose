import * as React from "react"
import { observable, observe, computed } from "mobx"
import { observer } from "mobx-react"

import { UUID } from "./names"
import * as C from "./keychart"
import * as E from "./editor"
import * as K from "./keymap"
import * as M from "./module"
import * as O from "./outline"
import * as P from "./project"
import * as S from "./stack"
import * as T from "./trees"

export class WorkspaceStore implements P.Resolver, M.Resolver {
  @observable projects :P.Project[] = []
  @observable selprojidx :number = 0
  @observable selcompidx :number = 0

  @observable openDefs :E.DefStore[] = []
  @observable seldefidx :number = -1

  // TODO: have a mode or dialog abstraction if we grow to include more of these...
  @observable creatingNew :boolean = false

  keymap = new K.Keymap()

  constructor () {
    observe(this, "seldefidx", change => {
      const odef = change.oldValue !== undefined && this.defAt(change.oldValue)
      const ndef = this.defAt(change.newValue)
      if (odef) odef.unbind(this.keymap)
      if (ndef) ndef.bind(this.keymap)
    })
  }

  @computed get selectedProject () :P.Project|void {
    const selidx = this.selprojidx, projs = this.projects
    return projs.length > selidx ? projs[selidx] : undefined
  }
  @computed get selectedComponent () :P.Component|void {
    const selidx = this.selcompidx, selproj = this.selectedProject
    const comps = selproj ? selproj.components : []
    return comps.length > selidx ? comps[selidx] : undefined
  }

  @computed get selectedDef () :E.DefStore|void { return this.defAt(this.seldefidx) }

  // the module in which a new def will be created: the one that owns the selected def, if we have a
  // selected def, or the first module otherwise
  get newDefMod () :M.Module {
    const seldef = this.selectedDef
    return seldef ? seldef.sym.mod : this.projects[0].components[0].modules[0]
  }

  defAt (idx :number) :E.DefStore|void {
    const stores = this.openDefs
    return stores.length > idx ? stores[idx] : undefined
  }

  moveSelection (delta :number) {
    const stores = this.openDefs.length
    this.seldefidx = (this.seldefidx+stores+delta)%stores
  }

  openDef (sym :M.DefSym) {
    for (let od of this.openDefs) {
      if (od.sym === sym) {
        this.seldefidx = this.openDefs.indexOf(od)
        // TODO: make sure selected def is scrolled into view
        return
      }
    }
    const ds = new E.DefStore(sym, sym.mod.tree(sym), this.keymap,
                              computed(() => this.selectedDef), () => {
      const idx = this.openDefs.indexOf(ds)
      if (idx >= 0) this.seldefidx = idx
    })
    // TODO: insert def after selected def?
    const idx = this.openDefs.length
    this.openDefs.push(ds)
    this.seldefidx = idx
  }

  // from P.Project
  resolveProject (uuid :UUID) :P.Project|void {
    // TEMP: linear search!
    for (let proj of this.projects) if (proj.uuid === uuid) return proj
  }

  // from M.Resolver
  resolveModule (uuid :UUID) :M.Module|void {
    // TEMP: linear search!
    for (let proj of this.projects) for (let comp of proj.components) {
      for (let mod of comp.modules) if (mod.uuid === uuid) return mod
    }
  }
}

@observer
export class Workspace extends React.Component<{store :WorkspaceStore}> implements K.Source {

  // from K.Source
  get name () :string { return "Workspace" }

  // from K.Source
  readonly mappings :K.Mapping[] = [{
    descrip: "Select previous def",
    chord: "M-ArrowUp",
    action: kp => this.props.store.moveSelection(-1)
  }, {
    descrip: "Select next def",
    chord: "M-ArrowDown",
    action: kp => this.props.store.moveSelection(1)
  }, {
    descrip: "Create new def",
    chord: "C-KeyN",
    action: kp => { this.props.store.creatingNew = true }
  }]

  // from K.Source
  handleKey (kp :K.KeyPress) :boolean {
    return false
  }

  dispatchKey :(ev :KeyboardEvent) => boolean = ev => {
    const store = this.props.store
    if (!store.creatingNew) {
      const handled = this.props.store.keymap.handleKey(ev)
      if (handled) ev.preventDefault()
      return handled
    }

    const mod = store.newDefMod
    let tree :T.DefTree|void = undefined
    switch (ev.code) {
    case "KeyF": tree = mod.addFunDef("") ; break
    case "KeyS": tree = mod.addSumDef("") ; break
    case "KeyP": tree = mod.addProdDef("") ; break
    case "KeyT": tree = mod.addTypeDef("") ; break // TODO: add alias def?
    case "Escape": break // fall through and clear dialog
    default: return false
    }
    if (tree) store.openDef(tree.sym as M.DefSym)
    store.creatingNew = false
    return true
  }

  componentWillMount() {
    document.addEventListener("keydown", this.dispatchKey, false)
    this.props.store.keymap.addSource(this)
  }
  componentWillUnmount() {
    document.removeEventListener("keydown", this.dispatchKey, false)
    this.props.store.keymap.removeSource(this)
  }

  render () {
    const store = this.props.store
    return (<div id="top">
              {store.creatingNew ? this.createDefPopup() : undefined}
              <O.Outline store={store} />
              <S.Stack store={store} />
              <C.KeyChart keymap={store.keymap} />
            </div>)
  }

  private createDefPopup () :JSX.Element {
    const store = this.props.store
    return (<div className="createDef">
              <div>Create new definition in module '{store.newDefMod.name}':</div>
              <ul>
                <li><span className="shortcut">f</span>unction</li>
                <li><span className="shortcut">s</span>um type</li>
                <li><span className="shortcut">p</span>roduct type</li>
                <li><span className="shortcut">t</span>ype alias</li>
              </ul>
            </div>)
  }
}
