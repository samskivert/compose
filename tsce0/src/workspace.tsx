import * as React from 'react'
import { observable, computed } from 'mobx'
import { observer } from 'mobx-react'

import * as E from './editor'
import * as O from './outline'
import * as S from './stack'
import * as T from './trees'

export class WorkspaceStore {
  @observable defs :E.DefStore[] = []
  @observable selectedDef :E.DefStore|void = undefined

  addDef (def :T.Def) {
    this.defs.push(new E.DefStore(def, computed(() => this.selectedDef)))
  }

  moveSelection (delta :number) {
    const selstore = this.selectedDef
    if (selstore) {
      const selidx = this.defs.indexOf(selstore)
      const stores = this.defs.length
      this.selectedDef = this.defs[(selidx+stores+delta)%stores]
    } else if (this.defs.length > 0) {
      this.selectedDef = this.defs[0]
    }
  }
}

@observer
export class Workspace extends React.Component<{store :WorkspaceStore}> {

  handleKey :(ev :KeyboardEvent) => boolean = ev => {
    if (ev.metaKey) {
      switch (ev.code) {
      case "ArrowUp": this.props.store.moveSelection(-1) ; return true
      case "ArrowDown": this.props.store.moveSelection(1) ; return true
      }
    }
    const store = this.props.store.selectedDef
    return store ? store.keyHandler(ev) : false
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
