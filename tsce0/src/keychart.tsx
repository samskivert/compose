import * as React from "react"
import { observer } from "mobx-react"
import * as K from "./keymap"

const codeGlyphs = new Map([
  ["ArrowLeft", "←"],
  ["ArrowRight", "→"],
  ["ArrowUp", "↑"],
  ["ArrowDown", "↓"],
  ["Backslash", "\\"],
  ["Minus", "-"],
])

const modGlyphs = new Map([
  ["C", "Ctrl"],
  ["A", "Alt"],
  ["M", "Meta"],
  ["S", "Shift"]
])

function codeToGlyph (code :string) {
  if (code.startsWith("Key")) return code.substring(3)
  const glyph = codeGlyphs.get(code)
  return glyph || code
}

function format (chord :string) :JSX.Element {
  const keys :string[] = []
  while (chord[1] === '-') {
    keys.push(modGlyphs.get(chord[0]) || chord[0])
    chord = chord.substring(2)
  }
  keys.push(codeToGlyph(chord))
  return <span className="keys">
    {keys.map(k => <span className="key">{k}</span>)}
  </span>
}

@observer
export class KeyChart extends React.Component<{keymap :K.Keymap}> {

  render () {
    const keymap = this.props.keymap
    const mappings :JSX.Element[] = []
    for (let source of keymap.sources) {
      for (let m of source.mappings) {
        if (keymap.chordToMapping.get(m.chord) === m) {
          mappings.push(<div key={m.chord} className="binding">
                        {format(m.chord)} - {m.descrip}</div>)
        }
      }
    }
    return <div className="keychart">{mappings}</div>
  }
}
