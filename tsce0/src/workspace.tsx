import * as React from "react"
import { observable, computed } from "mobx"
import { observer } from "mobx-react"

import { UUID } from "./names"
import * as E from "./editor"
import * as O from "./outline"
import * as S from "./stack"
import * as M from "./module"
import * as T from "./trees"
import * as P from "./project"

export class WorkspaceStore implements P.Resolver, M.Resolver {
  @observable projects :P.Project[] = []
  @observable selprojidx :number = 0
  @observable selcompidx :number = 0

  @observable openDefs :E.DefStore[] = []
  @observable seldefidx :number = 0

  // TODO: have a mode or dialog abstraction if we grow to include more of these...
  @observable creatingNew :boolean = false

  @computed get selectedProject () :P.Project|void {
    const selidx = this.selprojidx, projs = this.projects
    return projs.length > selidx ? projs[selidx] : undefined
  }
  @computed get selectedComponent () :P.Component|void {
    const selidx = this.selcompidx, selproj = this.selectedProject
    const comps = selproj ? selproj.components : []
    return comps.length > selidx ? comps[selidx] : undefined
  }

  @computed get selectedDef () :E.DefStore|void {
    const selidx = this.seldefidx, stores = this.openDefs
    return stores.length > selidx ? stores[selidx] : undefined
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
    const ds = new E.DefStore(sym, sym.mod.tree(sym), computed(() => this.selectedDef), () => {
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
export class Workspace extends React.Component<{store :WorkspaceStore}> {

  handleKey :(ev :KeyboardEvent) => boolean = ev => {
    const store = this.props.store
    if (store.creatingNew) {
      const seldef = store.selectedDef, mod = seldef ? seldef.sym.mod :
        store.projects[0].components[0].modules[0]
      let tree :T.DefTree|void = undefined
      switch (ev.code) {
      case "KeyF": tree = mod.addFunDef("") ; break
      case "KeyS": tree = mod.addSumDef("") ; break
      case "KeyP": tree = mod.addProdDef("") ; break
      case "KeyT": tree = mod.addTypeDef("") ; break // TODO: add alias def?
      case "Escape": break // fall through and clear dialog
      default: return false
      }
      if (tree) {
        store.openDef(tree.sym as M.DefSym)
      }
        // const seldef = store.selectedDef
        // if (seldef) {
        //   seldef.sym.mod.mkDefHole()
        //   // store.insertDef(store.seldefidx, P.testMod.mkDefHole(), true)
        //   ev.preventDefault()
        // }

      store.creatingNew = false
      return true

    } else if (ev.metaKey) {
      switch (ev.code) {
      case "ArrowUp": this.props.store.moveSelection(-1) ; return true
      case "ArrowDown": this.props.store.moveSelection(1) ; return true
      }

    } else if (ev.ctrlKey) {
      switch (ev.code) {
      case "KeyN":
        store.creatingNew = true
        return true
      }
    }
    const selDef = store.selectedDef
    return selDef ? selDef.keyHandler(ev) : false
  }

  componentWillMount() {
    document.addEventListener("keydown", this.handleKey, false)
  }
  componentWillUnmount() {
    document.removeEventListener("keydown", this.handleKey, false)
  }

  render () {
    const store = this.props.store
    return (<div id="top">
              {store.creatingNew ? this.createDefPopup() : undefined}
              <O.Outline store={store} />
              <S.Stack store={store} />
            </div>)
  }

  private createDefPopup () :JSX.Element {
    return (<div className="createDef">
              <div>Create new:</div>
              <ul>
                <li><span className="shortcut">f</span>unction</li>
                <li><span className="shortcut">s</span>um type</li>
                <li><span className="shortcut">p</span>roduct type</li>
                <li><span className="shortcut">t</span>ype alias</li>
              </ul>
            </div>)
  }
}
