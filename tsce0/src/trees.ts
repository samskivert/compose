import { Name } from "./names"
import { Type, Apply, Ctor, Var, typeUnknown } from "./types"
import { Constant, constTrue } from "./constants"

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

type Edit = {
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

  /** Returns an edited copy of this tree obtained by applying `edit` to the appropriate `Name`,
    * sub-`Expr` or `Type` to which `path` resolves. */
  edit (edit :Edit, path :Path) :Tree {
    switch (path.length) {
    case  0: throw new Error(`Invalid (empty) path for edit of ${this}`)
    case  1: return this.editSelf(edit, path[0])
    default: return this.editChild(edit, path[0], path.slice(1))
    }
  }

  /** Edits the name in `this` tree at `path` via `namefn`. */
  editName (namefn :(old :Name) => Name, path :Path) :Tree {
    return this.edit({namefn, exprfn: x => x, typefn: x => x}, path)
  }
  /** Edits the expr in `this` tree at `path` via `exprfn`. */
  editExpr (exprfn :(old :Expr) => Expr, path :Path) :Tree {
    return this.edit({namefn: x => x, exprfn, typefn: x => x}, path)
  }
  /** Edits the type in `this` tree at `path` via `typefn`. */
  editType (typefn :(old :Type) => Type, path :Path) :Tree {
    return this.edit({namefn: x => x, exprfn: x => x, typefn}, path)
  }

  toString () { return `${this.constructor.name}`}

  protected editSelf (edit :Edit, idx :number) :Tree {
    throw new Error(`Invalid self edit: ${this} @ ${idx}`)
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    throw new Error(`Invalid child edit: ${this} @ ${idx} with ${tail}`)
  }
}

// follow :: forall a. Path -> (Path -> Int -> a) -> a
// follow path idxfn = case uncons path of
//   Just {head, tail} -> idxfn tail head
//   Nothing -> idxfn [] 0 -- TODO: report failure for malformed path?

// firstEditable :: Expr -> Maybe Expr
// firstEditable expr = case expr of
//   Lit _ -> Just expr
//   Ref _ -> Just expr
//   Hole _ -> Just expr
//   App fun arg -> (firstEditable fun) <|> (firstEditable arg)
//   Let _ _ _ _ -> Just expr -- editing the let name
//   Abs _ _ _ -> Just expr -- editing the arg name
//   If test tx fx -> (firstEditable test) <|> (firstEditable tx) <|> (firstEditable fx)
//   Case scrut cases -> (firstEditable scrut) <|>
//     (foldl (\b a -> b <|> (firstEditable a)) Nothing cases)
//   CaseCase pat body -> (firstEditable pat) <|> (firstEditable body)

// ----------------
// Definition model
// ----------------

/** Definition AST. */
export abstract class Def extends Tree {

}

export class Term extends Def {
  readonly kind = "term"
  constructor (readonly name :Name, readonly expr :Expr) { super() }

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Term(edit.namefn(this.name), this.expr)
    // TODO: 1 -> return new Term(this.name, typefn(this.tpe), this.expr);
    default: return super.editSelf(edit, idx)
    }
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  2: return new Term(this.name, this.expr.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }

  toString () { return `Term:${this.name}`}
}

export class Union extends Def {
  readonly kind = "union"
  constructor (readonly name :Name, readonly records :Record[]) { super() }

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Union(edit.namefn(this.name), this.records)
    default: return super.editSelf(edit, idx)
    }
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    const cidx = idx-1
    if (cidx >= 0 && cidx < this.records.length) {
      const erecord = this.records[cidx].edit(edit, tail) as Record
      return new Union(this.name, update(this.records, cidx, erecord))
    } else {
      return super.editChild(edit, idx, tail)
    }
  }

  toString () { return `Union:${this.name}`}
}

export class Field extends Def {
  readonly kind = "field"
  constructor (readonly name :Name, readonly tpe :Type) { super() }

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Field(edit.namefn(this.name), this.tpe)
    case  1: return new Field(this.name, edit.typefn(this.tpe))
    default: return super.editSelf(edit, idx)
    }
  }
}

export class Record extends Def {
  readonly kind = "record"
  constructor (readonly name :Name, readonly fields :Field[]) { super() }

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Record(edit.namefn(this.name), this.fields)
    default: return super.editSelf(edit, idx)
    }
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    const cidx = idx-1
    if (cidx >= 0 && cidx < this.fields.length) {
      const efield = this.fields[cidx].edit(edit, tail) as Field
      return new Record(this.name, update(this.fields, cidx, efield))
    } else {
      return super.editChild(edit, idx, tail)
    }
  }

  toString () { return `Union:${this.name}`}
}

// ----------------
// Expression model
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

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Ref(edit.namefn(this.name))
    default: return super.editSelf(edit, idx)
    }
  }

  toString () { return `Ref:${this.name}`}
}
export class Hole extends Expr {
  readonly kind = "hole"
  constructor (readonly tpe :Type) { super() }

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Hole(edit.typefn(this.tpe))
    default: return super.editSelf(edit, idx)
    }
  }

  toString () { return `Hole:${this.tpe}`}
}

export class App extends Expr {
  readonly kind = "app"
  constructor (readonly fun :Expr, readonly arg :Expr) { super() }

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

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Let(edit.namefn(this.name), this.tpe, this.value, this.body)
    case  1: return new Let(this.name, edit.typefn(this.tpe), this.value, this.body)
    default: return super.editSelf(edit, idx)
    }
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  2: return new Let(this.name, this.tpe, this.value.edit(edit, tail), this.body)
    case  3: return new Let(this.name, this.tpe, this.value, this.body.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }

  toString () { return `Let:${this.name}`}
}

export class Abs extends Expr {
  readonly kind = "abs"
  constructor (readonly name :Name, readonly tpe :Type, readonly body :Expr) { super() }

  protected editSelf (edit :Edit, idx :number) :Tree {
    switch (idx) {
    case  0: return new Abs(edit.namefn(this.name), this.tpe, this.body)
    case  1: return new Abs(this.name, edit.typefn(this.tpe), this.body)
    default: return super.editSelf(edit, idx)
    }
  }

  protected editChild (edit :Edit, idx :number, tail :Path) :Tree {
    switch (idx) {
    case  2: return new Abs(this.name, this.tpe, this.body.edit(edit, tail))
    default: return super.editChild(edit, idx, tail)
    }
  }

  toString () { return `Abs:${this.name}`}
}

export class If extends Expr {
  readonly kind = "if"
  constructor (readonly test :Expr, readonly texp :Expr, readonly fexp :Expr) { super() }

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
export const letExample = new Term("foo", new Lit(constTrue))

// -- reverse as :List A -> List A =
// --   let revacc as acc = case as of
// --     Nil -> acc
// --     Cons h t -> revacc t h :: acc
// --   in revacc as Nil
const tpListA = new Apply(new Ctor("List"), new Var("A"))
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
  new Abs(
    "as",
    tpListA,
    new Let(
      "revacc",
      typeUnknown,
      revAccDef,
      new App(
        new Ref("revacc"),
        new App(new Ref("as"), new Ref("Nil"))
      )
    )
  )
)
