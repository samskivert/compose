import { Name } from "./names"
import { Tag, Constant, constTrue } from "./constants"

// ---------------
// Tree path model
// ---------------

// A path identifies a point in an AST, generally used for editing. The editing cursor uses paths to
// track the insertion point. The selection is a pair of paths that track the start and end of the
// selection.

/**
 * Identifies a path from some root node to a field of some target node. Each path component is the
 * index of the field in the current node. If a path has additional components that field must
 * represent another AST node, but the final component of a path may reference a terminal field like
 * a `Name` or `Constant`.
 *
 * Note that a path will generally start in a `Def` node and may traverse into `Expr` nodes en route
 * to its terminal node (which may itself be a `Def` node, an `Expr` node, a `Name`, etc.). Thus we
 * cannot, in general, obtain the "thing" to which a path points as there is no single type that
 * describes it.
 */
export type Path = number[]

export const emptyPath :Path = []
export const extendPath = (path :Path, idx :number) :Path => path.concat([idx])
export const pathsEqual = (pa :Path, pb :Path) :boolean => {
  if (pa.length !== pb.length) return false
  for (let ii = 0; ii < pa.length; ii += 1) if (pa[ii] !== pb[ii]) return false
  return true
}

export type Edit = {
  namefn :(old :Name) => Name,
  exprfn :(old :Expr) => Expr,
  typefn :(old :Type) => Type }

// ---
// AST
// ---

/** Abstracts over editable trees: `Def` and `Expr` (and maybe someday `Type`?). */
export abstract class Tree {

  // A tree edit is a function from tree to tree. We provide methods to construct
  // edits out of paths and subtrees, or edit operations to existing tree nodes.

  firstEditable (pre :Path = []) :Path {
    return pre
  }

  /** Returns an edited copy of this tree obtained by applying `edit` to the appropriate `Name`,
    * sub-`Expr` or `Type` to which `path` resolves. */
  edit (edit :Edit, path :Path) :Tree {
    if (path.length == 0) throw new Error(`Invalid (empty) path for edit of ${this}`)
    return this.editChild(edit, path[0], path.slice(1))
  }

  /** Edits the name in `this` tree at `path` via `namefn`. */
  editName (namefn :(old :Name) => Name, path :Path) :Tree {
    console.log(`${this} :: editName @ ${path}`)
    return this.edit({namefn, exprfn: x => x, typefn: x => x}, path)
  }
  /** Edits the expr in `this` tree at `path` via `exprfn`. */
  editExpr (exprfn :(old :Expr) => Expr, path :Path) :Tree {
    console.log(`${this} :: editExpr @ ${path}`)
    return this.edit({namefn: x => x, exprfn, typefn: x => x}, path)
  }
  /** Edits the type in `this` tree at `path` via `typefn`. */
  editType (typefn :(old :Type) => Type, path :Path) :Tree {
    console.log(`${this} :: editType @ ${path}`)
    return this.edit({namefn: x => x, exprfn: x => x, typefn}, path)
  }

  toString () { return `${this.constructor.name}`}

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    throw new Error(`Invalid child edit: ${this} @ ${idx} with ${tail}`)
  }
}

// ----------
// Type trees
// ----------

export abstract class Type extends Tree {

  edit (edit :Edit, path :Path) :Type {
    return (path.length == 0) ? edit.typefn(this) : super.edit(edit, path)
  }
}

export class Unknown extends Type {
  readonly kind = "unknown"
}
export const typeUnknown :Unknown = new Unknown()

export class Const extends Type {
  readonly kind = "const"
  constructor (readonly cnst :Constant) { super() }
  toString () { return `Const:${this.cnst}`}
}

export class Data extends Type {
  readonly kind = "data"
  constructor (readonly tag :Tag, readonly size :number) { super () }
  toString () { return `Data:${this.tag}${this.size}`}
}

export class TRef extends Type {
  readonly kind = "tref"
  constructor (readonly name :Name) { super() }
  toString () { return `TRef:${this.name}`}
}

export class TVar extends Type {
  readonly kind = "tvar"
  constructor(readonly name :Name) { super () }
  toString () { return `TVar:${this.name}`}
}

export class Arrow extends Type {
  readonly kind = "arrow"
  constructor (readonly from :Type, readonly to :Type) { super() }
  toString () { return `${this.from} -> ${this.to}`}

  firstEditable (pre :Path = []) :Path {
    return this.from.firstEditable(extendPath(pre, 0))
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new Arrow(this.from.edit(edit, tail), this.to)
    case  1: return new Arrow(this.from, this.to.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class TApp extends Type {
  readonly kind = "tapp"
  constructor (readonly ctor :Type, readonly arg :Type) { super () }
  toString () { return `${this.ctor} ${this.arg}`}

  firstEditable (pre :Path = []) :Path {
    return this.ctor.firstEditable(extendPath(pre, 0))
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new TApp(this.ctor.edit(edit, tail), this.arg)
    case  1: return new TApp(this.ctor, this.arg.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

// TODO
// -- | Array Type
// -- | Record Name Params Fields
// -- | Field Name Type
// -- | Union Params Cases
// -- | Interface Name Params Methods
// -- | Method Name Type

// ----------------
// Definition trees
// ----------------

/** Definition AST. */
export abstract class Def extends Tree {

}

export class Term extends Def {
  readonly kind = "term"
  constructor (readonly name :Name, readonly tpe :Type, readonly expr :Expr) { super() }
  toString () { return `Term:${this.name}`}

  firstEditable (pre :Path = []) :Path {
    return extendPath(pre, 0)
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new Term(edit.namefn(this.name), this.tpe, this.expr)
    case  1: return new Term(this.name, this.tpe.edit(edit, tail), this.expr);
    case  2: return new Term(this.name, this.tpe, this.expr.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class Union extends Def {
  readonly kind = "union"
  constructor (readonly name :Name, readonly records :Record[]) { super() }
  toString () { return `Union:${this.name}`}

  firstEditable (pre :Path = []) :Path {
    return extendPath(pre, 0)
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    if (idx == 0) return new Union(edit.namefn(this.name), this.records)
    const cidx = idx-1
    if (cidx >= 0 && cidx < this.records.length) {
      const erecord = this.records[cidx].edit(edit, tail) as Record
      return new Union(this.name, update(this.records, cidx, erecord))
    } else {
      return super.editChild(edit, idx, tail)
    }
  }
}

export class Field extends Def {
  readonly kind = "field"
  constructor (readonly name :Name, readonly tpe :Type) { super() }
  toString () { return `Field:${this.name}`}

  firstEditable (pre :Path = []) :Path {
    return extendPath(pre, 0)
  }

  protected editChild (edit :Edit, idx :number, tail :number[]) :Tree {
    switch (idx) {
    case  0: return new Field(edit.namefn(this.name), this.tpe)
    case  1: return new Field(this.name, this.tpe.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class Record extends Def {
  readonly kind = "record"
  constructor (readonly name :Name, readonly fields :Field[]) { super() }
  toString () { return `Record:${this.name}`}

  firstEditable (pre :Path = []) :Path {
    return extendPath(pre, 0)
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    if (idx == 0) return new Record(edit.namefn(this.name), this.fields)
    const cidx = idx-1
    if (cidx >= 0 && cidx < this.fields.length) {
      const efield = this.fields[cidx].edit(edit, tail) as Field
      return new Record(this.name, update(this.fields, cidx, efield))
    } else {
      return super.editChild(edit, idx, tail)
    }
  }
}

// ----------------
// Expression trees
// ----------------

// TODO: factor `type Binding = {name :: Name, tpe :: Type, value :: Expr}` out of Let/Abs?
// TODO: create doc AST, add Doc node to every node that defines a name (Def, Let, Abs, etc.)

/** Expression AST. */
export abstract class Expr extends Tree {

  edit (edit :Edit, path :Path) :Expr {
    return (path.length == 0) ? edit.exprfn(this) : super.edit(edit, path)
  }
}

export class Lit extends Expr {
  readonly kind = "lit"
  constructor (readonly cnst :Constant) { super() }
  toString () { return `Lit:${this.cnst}`}
}

export class Ref extends Expr {
  readonly kind = "ref"
  constructor (readonly name :Name) { super() }
  toString () { return `Ref:${this.name}`}

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new Ref(edit.namefn(this.name))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class Hole extends Expr {
  readonly kind = "hole"
  constructor (readonly tpe :Type) { super() }
  toString () { return `Hole:${this.tpe}`}
}

export const exprHole = new Hole(typeUnknown)

export class App extends Expr {
  readonly kind = "app"
  constructor (readonly fun :Expr, readonly arg :Expr) { super() }

  firstEditable (pre :Path = []) :Path {
    return this.fun.firstEditable(extendPath(pre, 0))
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new App(this.fun.edit(edit, tail), this.arg)
    case  1: return new App(this.fun, this.arg.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class Let extends Expr {
  readonly kind ="let"
  constructor (readonly name :Name, readonly tpe :Type, readonly value :Expr,
               readonly body :Expr) { super() }
  toString () { return `Let:${this.name}`}

  firstEditable (pre :Path = []) :Path {
    return extendPath(pre, 0)
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new Let(edit.namefn(this.name), this.tpe, this.value, this.body)
    case  1: return new Let(this.name, this.tpe.edit(edit, tail), this.value, this.body)
    case  2: return new Let(this.name, this.tpe, this.value.edit(edit, tail), this.body)
    case  3: return new Let(this.name, this.tpe, this.value, this.body.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class Abs extends Expr {
  readonly kind = "abs"
  constructor (readonly name :Name, readonly tpe :Type, readonly body :Expr) { super() }
  toString () { return `Abs:${this.name}`}

  firstEditable (pre :Path = []) :Path {
    return extendPath(pre, 0)
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new Abs(edit.namefn(this.name), this.tpe, this.body)
    case  1: return new Abs(this.name, this.tpe.edit(edit, tail), this.body)
    case  2: return new Abs(this.name, this.tpe, this.body.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class If extends Expr {
  readonly kind = "if"
  constructor (readonly test :Expr, readonly texp :Expr, readonly fexp :Expr) { super() }

  firstEditable (pre :Path = []) :Path {
    return this.test.firstEditable(extendPath(pre, 0))
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new If(this.test.edit(edit, tail), this.texp, this.fexp)
    case  1: return new If(this.test, this.texp.edit(edit, tail), this.fexp)
    case  1: return new If(this.test, this.texp, this.fexp.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

export class Case extends Expr {
  readonly kind = "case"
  constructor (readonly scrut :Expr, readonly cases :Expr[]) { super() }

  firstEditable (pre :Path = []) :Path {
    return this.scrut.firstEditable(extendPath(pre, 0))
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    const cidx = idx-1
    if (idx == 0) return new Case(this.scrut.edit(edit, tail), this.cases)
    else if (cidx < this.cases.length) {
      const ecase = this.cases[cidx].edit(edit, tail)
      return new Case(this.scrut, update(this.cases, cidx, ecase))
    } else return super.editChild(edit, idx, tail)
  }
}

export class CaseCase extends Expr {
  readonly kind = "casecase"
  constructor (readonly pat :Expr, // TODO: this should be a pattern tree
               readonly body :Expr) { super() }

  firstEditable (pre :Path = []) :Path {
    return this.pat.firstEditable(extendPath(pre, 0))
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  0: return new CaseCase(this.pat.edit(edit, tail), this.body)
    case  1: return new CaseCase(this.pat, this.body.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }
}

function update<A> (elems :A[], idx :number, elem :A) :A[] {
  const nelems = elems.slice(0)
  nelems[idx] = elem
  return nelems
}

// --   | Cond Array CondCase
// --   | CondCase Expr Expr

// ----------
// Test trees
// ----------

// -- let foo = true
export const letExample = new Term("foo", new Data(Tag.Bool, 1), new Lit(constTrue))

// -- reverse as :List A -> List A =
// --   let revacc as acc = case as of
// --     Nil -> acc
// --     Cons h t -> revacc t h :: acc
// --   in revacc as Nil
const tpListA = new TApp(new TRef("List"), new TVar("A"))
const revAccDef = new Abs(
  "as",
  tpListA,
  new Abs(
    "acc",
    tpListA,
    new Case(
      new Ref("as"), [
        new CaseCase(
          new Ref("Nil"),
          new Hole(typeUnknown)
        ),
        new CaseCase(
          new App(new App(new Ref("Cons"), new Ref("h")), new Ref("t")),
          new App(
            new App(new Ref("revacc"), new Ref("t")),
            new App(new App(new Ref("Cons"), new Ref("h")), new Ref("acc"))
          )
        )
      ]
    )
  )
)
export const revExample = new Term(
  "reverse",
  tpListA,
  new Abs(
    "as",
    tpListA,
    new Let(
      "revacc",
      tpListA,
      revAccDef,
      new App(
        new Ref("revacc"),
        new App(new Ref("as"), new Ref("Nil"))
      )
    )
  )
)
