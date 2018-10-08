import { UUID } from "./names"

// TEMP: just a simple way to store ASTs between sessions for now
export abstract class Store {
  abstract contains (uuid :UUID) :boolean
  abstract load (uuid :UUID) :object|void
  abstract store (uuid :UUID, value :object) :void
  abstract clear () :void
}

export class LocalStore {
  contains (uuid :UUID) :boolean {
    return localStorage.getItem(uuid) !== null
  }
  load (uuid :UUID) :object|void {
    const enc = localStorage.getItem(uuid)
    return enc && JSON.parse(enc)
  }
  store (uuid :UUID, value :object) {
    const enc = JSON.stringify(value)
    console.log(`Storing ${uuid} ==> ${enc}`)
    localStorage.setItem(uuid, enc)
  }
  clear () {
    localStorage.clear()
  }
}

export class MemoryStore {
  constructor (private map :Map<UUID, object>) {}
  contains (uuid :UUID) :boolean { return this.map.has(uuid) }
  load (uuid :UUID) :object|void { return this.map.get(uuid) }
  store (uuid :UUID, value :object) { this.map.set(uuid, value) }
  clear () { this.map.clear() }
}
