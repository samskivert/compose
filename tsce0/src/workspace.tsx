import * as React from 'react'
import { observable, computed } from 'mobx'
import { observer } from 'mobx-react'

import * as E from './editor'
import * as O from './outline'
import * as S from './stack'
import * as T from './trees'

export class WorkspaceStore {
  @observable defs :E.DefStore[] = []
  @observable selidx :number = 0

  @computed get selectedDef () :E.DefStore|void {
    return this.defs.length > 0 ? this.defs[this.selidx] : undefined
  }

  addDef (def :T.DefTree, editing = false) {
    this.insertDef(this.defs.length, def, editing)
  }

  insertDef (index :number, def :T.DefTree, editing = false) {
    this.defs.splice(index, 0, new E.DefStore(def, computed(() => this.selectedDef), editing))
  }

  moveSelection (delta :number) {
    const stores = this.defs.length
    this.selidx = (this.selidx+stores+delta)%stores
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
    } else if (ev.ctrlKey) {
      switch (ev.code) {
      case "KeyN":
        store.insertDef(store.selidx, T.mkDefHole(T.testModSym), true)
        ev.preventDefault()
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
    return (<div id="top">
              <O.Outline store={this.props.store} />
              <S.Stack store={this.props.store} />
            </div>)
  }
}
