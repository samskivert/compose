import * as React from 'react'
import { observable, computed } from 'mobx'
import { observer } from 'mobx-react'

import * as E from './editor'
import * as O from './outline'
import * as S from './stack'
import * as M from './module'
import * as P from './project'

export class WorkspaceStore implements M.Resolver {
  @observable projects :P.Project[] = []
  @observable selprojidx :number = 0
  @observable selcompidx :number = 0

  @observable openDefs :E.DefStore[] = []
  @observable seldefidx :number = 0

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

  // from M.Resolver
  resolve (uuid :M.UUID) :M.Module|void {
    // TEMP: linear search!
    for (let proj of this.projects) {
      for (let comp of proj.components) {
        for (let mod of comp.modules) {
          if (mod.uuid === uuid) return mod
        }
      }
    }
  }
}

@observer
export class Workspace extends React.Component<{store :WorkspaceStore}> {

  handleKey :(ev :KeyboardEvent) => boolean = ev => {
    const store = this.props.store
    if (ev.metaKey) {
      switch (ev.code) {
      case "ArrowUp": this.props.store.moveSelection(-1) ; return true
      case "ArrowDown": this.props.store.moveSelection(1) ; return true
      }
    // } else if (ev.ctrlKey) {
    //   switch (ev.code) {
    //   case "KeyN":
    //     store.insertDef(store.seldefidx, P.testMod.mkDefHole(), true)
    //     ev.preventDefault()
    //     return true
    //   }
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
    return (<div id="top">
              <O.Outline store={this.props.store} />
              <S.Stack store={this.props.store} />
            </div>)
  }
}
