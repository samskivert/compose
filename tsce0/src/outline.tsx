import * as React from 'react';
import { observer } from 'mobx-react'

import * as M from './module'
import * as W from './workspace'

@observer
export class Outline extends React.Component<{store :W.WorkspaceStore}> {

  render () {
    const store = this.props.store
    return (
      <div className="outline">
      {store.projects.map((proj, pp) => proj.components.map((comp, cc) => comp.modules.map((mod, mm) =>
        <div key={proj.uuid}>
          <div className="header">{proj.name} : {comp.name} : {mod.name}</div>
          <div className="defs">{mod.defs.map(sym => this.renderDef(sym))}
          </div>
        </div>
      )))}
      </div>)
  }

  renderDef (sym :M.DefSym) :JSX.Element {
    return <div key={sym.id} onClick={ev => this.props.store.openDef(sym)}>{sym.displayName}</div>
  }
}
