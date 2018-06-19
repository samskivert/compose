import { Constant, Tag } from "./constants"
import { Name } from "./names"

export abstract class Type {}

export class Unknown extends Type {
  readonly kind = "unknown"
}
export const typeUnknown :Unknown = new Unknown()

export class Const extends Type {
  readonly kind = "const"
  constructor (readonly cnst :Constant) { super() }
}

export class Data extends Type {
  readonly kind = "data"
  constructor (readonly tag :Tag, readonly size :number) { super () }
}

export class Arrow extends Type {
  readonly kind = "arrow"
  constructor (readonly from :Type, readonly to :Type) { super() }
}

export class Ctor extends Type {
  readonly kind = "ctor"
  constructor (readonly name :Name) { super() }
}

export class Var extends Type {
  readonly kind = "var"
  constructor(readonly name :Name) { super () }
}

export class Apply extends Type {
  readonly kind = "apply"
  constructor (readonly ctor :Type, readonly arg :Type) { super () }
}

// TODO
// -- | Array Type
// -- | Record Name Params Fields
// -- | Field Name Type
// -- | Union Params Cases
// -- | Interface Name Params Methods
// -- | Method Name Type
