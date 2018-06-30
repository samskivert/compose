import * as React from 'react';
import { observable, computed } from 'mobx'
import { observer } from 'mobx-react'

import * as E from './editor'
import * as O from './outline'
import * as S from './stack'
import * as T from './trees'

export class WorkspaceStore {
  @observable defStores :E.DefStore[] = []
  @observable selectedDef? :T.Def = undefined

  addDef (def :T.Def) {
    const isSelected = computed(() => this.selectedDef === def)
    this.defStores.push(new E.DefStore(def, isSelected))
  }
}

@observer
export class Workspace extends React.Component<{store :WorkspaceStore}> {

  render () {
    return (<div id="top">
              <O.Outline store={this.props.store} />
              <S.Stack store={this.props.store} />
            </div>)
  }
}
