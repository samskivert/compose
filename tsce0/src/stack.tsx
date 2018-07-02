import * as React from 'react';
import { observer } from 'mobx-react'

import * as E from './editor'
import * as W from './workspace'

@observer
export class Stack extends React.Component<{store :W.WorkspaceStore}> {

  render () {
    return (<div className="stack">{
      this.props.store.defs.map(store => <E.DefEditor store={store} />)
    }</div>)
  }
}
