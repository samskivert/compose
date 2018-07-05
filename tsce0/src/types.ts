import { Name } from "./names"
import { Tag, Constant } from "./constants"

export abstract class Type {
}

export class Hole extends Type {
  toString () { return `Hole` }
}
export const hole = new Hole()

export class Const extends Type {
  constructor (readonly cnst :Constant) { super() }
  toString () { return `Const:${this.cnst}`}
}

export class Arrow extends Type {
  constructor (readonly from :Type, readonly to :Type) { super() }
  toString () { return `${this.from} → ${this.to}`}
}

export class App extends Type {
  constructor (readonly ctor :Type, readonly arg :Type) { super () }
  toString () { return `${this.ctor} ${this.arg}`}
}

export class Abs extends Type {
  constructor (readonly name :Name, readonly body :Type) { super() }
  toString () { return `Abs:${this.name}`}
}

export class Scalar extends Type {
  constructor (readonly tag :Tag, readonly size :number) { super () }
  toString () { return `Scalar:${this.tag}${this.size}`}
}

export class Prod extends Type {
  constructor (readonly fields :Def[]) { super() }
  toString () { return `Record${this.fields.length}` }
}

export class Sum extends Type {
  constructor (readonly cases :Def[]) { super() }
  toString () { return `Sum${this.cases.length}` }
}

export class Def extends Type {
  constructor (readonly name :Name, readonly type :Type) { super() }
  toString () { return `${this.name}=${this.type}`}
}

// -- | Array Type?
// -- | Interface Name Params Methods
// -- | Method Name Type
