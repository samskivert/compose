import { observable, computed } from "mobx"

/** Identifies a key press: the name of the key, possibly prefixed by a modifier tag.
  * Examples: `S-Space`, `C-Return`, `Backspace`, `M`, `Shift-1`. */
export type Chord = string

/** Information on a key press (with possible modifiers). */
export type KeyPress = {
  /** The 'chord' identifying this key press. */
  readonly chord :Chord
  /** The printable representation of the pressed key, if one exists. Otherwise one of a set of
    * pre-defined key values. See:
    * https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values */
  readonly key :string
  /** Whether or not the keypress yields a printable character. */
  readonly isPrintable :boolean
  /** True if this key press is itself a modifier key (shift, ctrl, alt, meta). */
  readonly isModifier :boolean
  /** True if this key press happened while one or more modifier keys was pressed. */
  readonly isModified :boolean
  /** Prevents the default (web browser) handling of the event that generated this press. */
  readonly preventDefault :() => void
}

// TODO: for printable characters, I'd like the printable character to be the chord so that we need
// not rely on special keymap knowledge (like that `:` is `S-Semicolon`)
function mkChord (ev :KeyboardEvent) :Chord {
  let chord = ev.code
  if (ev.metaKey) chord = `M-${chord}`
  if (ev.altKey) chord = `A-${chord}`
  if (ev.ctrlKey) chord = `C-${chord}`
  if (ev.shiftKey) chord = `S-${chord}`
  return chord
}

const modCodes = new Set([
  "Shift", "ShiftLeft", "ShiftRight",
  "Control", "ControlLeft", "ControlRight",
  "Meta", "MetaLeft", "MetaRight",
  "Alt", "AltLeft", "AltRight"
])

/** Creates a `KeyPress` from a browser `KeyboardEvent`. */
export function mkKeyPress (ev :KeyboardEvent) :KeyPress {
  const isModified = ev.metaKey || ev.altKey || ev.ctrlKey || ev.shiftKey
  return {
    chord: mkChord(ev),
    key: ev.key,
    isPrintable: /*!isModified &&*/ ev.key.length === 1, // TODO: this is bullshit
    isModifier: modCodes.has(ev.key),
    isModified: isModified,
    preventDefault: () => ev.preventDefault()
  }
}

/** A single key mapping: an action triggered by a key chord. */
export type Mapping = {
  id :string
  descrip :string
  chord :Chord
  isEdit :() => boolean
  action :(kp :KeyPress) => void
}

/** A source of key mappings. This corresponds to some user interface element: the editor for the
  * selected def, the editor for the span under the cursor, the workspace itself. */
export interface Source {
  readonly name :string
  readonly mappings :Mapping[]
  willDispatch (kp :KeyPress, mp :Mapping) :boolean
  handleKey (kp :KeyPress) :boolean
}

/** Manages the active set of key mappings and their sources. */
export class Keymap {

  /** The list of active key mapping sources. */
  @observable sources :Source[] = []

  /** A list of all active key mappings, sorted alphabetically by chord. */
  @computed get mappings () :Mapping[] {
    const chords = Array.from(this.chordToMapping.values())
    chords.sort((a, b) => a.chord.localeCompare(b.chord))
    return chords
  }

  /** A map from chord to mapping for all active mappings. */
  @computed get chordToMapping () :Map<Chord,Mapping> {
    const mappings :Map<Chord,Mapping> = new Map()
    for (let source of this.sources) {
      for (let map of source.mappings) mappings.set(map.chord, map)
    }
    return mappings
  }

  addSource (source :Source) {
    this.sources.push(source)
  }

  removeSource (source :Source) {
    const idx = this.sources.indexOf(source)
    if (idx >= 0) this.sources.splice(idx, 1)
    else console.warn(`removeSource unknown source: '${source.name}' ` +
                      `(have ${this.sources.map(s => s.name)})`)
  }

  handleKey (ev :KeyboardEvent) :boolean {
    const kp = mkKeyPress(ev)
    const mapping = this.chordToMapping.get(kp.chord)
    if (mapping) {
      for (let source of this.sources) if (source.willDispatch(kp, mapping)) return true
      mapping.action(kp)
      return true
    }
    for (let source of this.sources) if (source.handleKey(kp)) return true
    return false
  }
}
