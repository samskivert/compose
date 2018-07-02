import * as React from 'react';
import { observer } from 'mobx-react'

import * as W from './workspace'

@observer
export class Outline extends React.Component<{store :W.WorkspaceStore}> {

  render () {
    const {defs} = this.props.store
    return (<div className="outline">{defs.map(dstore => {
      const cname = dstore.isActive ? "defs selected" : "defs"
      return <div className={cname}>{dstore.def.name}</div>
    })}</div>)
  }
}
