import * as React from "react"
import { observer } from "mobx-react"
import * as K from "./keymap"

@observer
export class KeyChart extends React.Component<{keymap :K.Keymap}> {

  render () {
    return <div className="keychart">
      {this.props.keymap.mappings.map(m => <div>{m.chord} - {m.descrip}</div>)}
    </div>
  }
}
