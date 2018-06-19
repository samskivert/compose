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

// ---
// AST
// ---

/** Abstracts over editable trees: `Def` and `Expr` (and maybe someday `Type`?). */
export abstract class Tree {

  // A tree edit is a function from tree to tree. We provide methods to construct
  // edits out of paths and subtrees, or edit operations to existing tree nodes.

  /** Edits this tree at `path`, applying `namefn` if `path` resolves to a `Name`, `exprfn` if
    * `path` resolves to an `Expr` and `typefn` if `path` resolves to a `Type`. */
  edit (namefn :(old :Name) => Name, exprfn :(old :Expr) => Expr, typefn :(old :Type) => Type,
        path :Path) :Tree {
    return (path.length == 0) ?
      this.editSelf(namefn, exprfn, typefn) :
      this.editAt(namefn, exprfn, typefn, path[0], path.slice(1))
  }

  /** Edits the name in `this` tree at `path` via `namefn`. */
  editName (namefn :(old :Name) => Name, path :Path) :Tree {
    return this.edit(namefn, x => x, x => x, path)
  }
  /** Edits the expr in `this` tree at `path` via `exprfn`. */
  editExpr (exprfn :(old :Expr) => Expr, path :Path) :Tree {
    return this.edit(x => x, exprfn, x => x, path)
  }
  /** Edits the type in `this` tree at `path` via `typefn`. */
  editType (typefn :(old :Type) => Type, path :Path) :Tree {
    return this.edit(x => x, x => x, typefn, path)
  }

  protected abstract editSelf (namefn :(old :Name) => Name,
                               exprfn :(old :Expr) => Expr,
                               typefn :(old :Type) => Type) :Tree

  protected abstract editAt (namefn :(old :Name) => Name,
                             exprfn :(old :Expr) => Expr,
                             typefn :(old :Type) => Type,
                             idx :number, tail :Path) :Tree
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

export type FieldDef = {name :Name, tpe :Type}
export type RecordDef = {name :Name, fields :FieldDef[]}

/** Definition AST. */
export abstract class Def extends Tree {

  protected editSelf (namefn :(old :Name) => Name, exprfn :(old :Expr) => Expr,
                      typefn :(old :Type) => Type) :Tree {
    return this
  }
}

export class Term extends Def {
  readonly kind = "term"
  constructor (readonly name :Name, readonly expr :Expr) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    switch (idx) {
    case 0: return new Term(namefn(this.name), this.expr);
    // TODO: 1 -> return new Term(this.name, typefn(this.tpe), this.expr);
    case 2: return new Term(this.name, this.expr.edit(namefn, exprfn, typefn, tail) as Expr);
    default: throw new Error(`Invalid edit index $idx: $this`)
    }

    return this
  }
}

function editField (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    path :Path, field :FieldDef) :FieldDef {
  if (path.length != 1) throw new Error(`Invalid path for field edit $field @ $path`)
  let {name, tpe} = field
  switch (path[0]) {
  case 0: return {name: namefn(name), tpe}
  case 1: return {name, tpe: typefn(tpe)}
  default: throw new Error(`Invalid path for field edit $field @ $path`)
  }
}

function editRecord (namefn :(old :Name) => Name,
                     exprfn :(old :Expr) => Expr,
                     typefn :(old :Type) => Type,
                     idx :number, tail :Path, record :RecordDef) :RecordDef {
  let {name, fields} = record
  switch (idx) {
  case 0:
    return {name: namefn(name), fields}
  default:
    let efield = editField(namefn, exprfn, typefn, tail, fields[idx-1])
    return {name, fields: fields.splice(idx-1, 1, efield)}
  }
}

export class Union extends Def {
  readonly kind = "union"
  constructor (readonly name :Name, readonly records :RecordDef[]) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
  //       0 -> Union (namefn name) records
  //       _ -> case modifyAt (idx-1) (editRecord tail) records of
  //         Just nrecords -> Union name nrecords
  //         Nothing -> def
    return this
  }
}

export class Record extends Def {
  readonly kind = "record"
  constructor (readonly record :RecordDef) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    return new Record(editRecord(namefn, exprfn, typefn, idx, tail, this.record))
  }
}

// ----------------
// Expression model
// ----------------

// TODO: factor `type Binding = {name :: Name, tpe :: Type, value :: Expr}` out of Let/Abs?
// TODO: create doc AST, add Doc node to every node that defines a name (Def, Let, Abs, etc.)

/** Expression AST. */
export abstract class Expr extends Tree {

  protected editSelf (namefn :(old :Name) => Name, exprfn :(old :Expr) => Expr,
                      typefn :(old :Type) => Type) :Tree {
    return exprfn(this)
  }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    throw new Error(`Invalid path for expr edit: $this @ $idx ++ $tail`)
  }
}

export class Lit extends Expr {
  readonly kind = "lit"
  constructor (readonly cnst :Constant) { super() }
}

export class Ref extends Expr {
  readonly kind = "ref"
  constructor (readonly name :Name) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    switch (idx) {
    case 0: return new Ref(namefn(this.name))
    default: return super.editAt(namefn, exprfn, typefn, idx, tail)
    }
  }
}

export class Hole extends Expr {
  readonly kind = "hole"
  constructor (readonly tpe :Type) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    switch (idx) {
    case 0: return new Hole(typefn(this.tpe))
    default: return super.editAt(namefn, exprfn, typefn, idx, tail)
    }
  }
}

export class App extends Expr {
  readonly kind = "app"
  constructor (readonly fun :Expr, readonly arg :Expr) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    switch (idx) {
    case 0: return new App(exprfn(this.fun), this.arg)
    case 1: return new App(this.fun, exprfn(this.arg))
    default: return super.editAt(namefn, exprfn, typefn, idx, tail)
    }
  }
}

export class Let extends Expr {
  readonly kind ="let"
  constructor (readonly name :Name, readonly tpe :Type, readonly value :Expr,
               readonly body :Expr) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    switch (idx) {
    case 0: return new Let(namefn(this.name), this.tpe, this.value, this.body)
    case 1: return new Let(this.name, typefn(this.tpe), this.value, this.body)
    case 2: return new Let(this.name, this.tpe, exprfn(this.value), this.body)
    case 3: return new Let(this.name, this.tpe, this.value, exprfn(this.body))
    default: return super.editAt(namefn, exprfn, typefn, idx, tail)
    }
  }
}

export class Abs extends Expr {
  readonly kind = "abs"
  constructor (readonly name :Name, readonly tpe :Type, readonly body :Expr) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
    switch (idx) {
    case 0: return new Abs(namefn(this.name), this.tpe, this.body)
    case 1: return new Abs(this.name, typefn(this.tpe), this.body)
    case 3: return new Abs(this.name, this.tpe, exprfn(this.body))
    default: return super.editAt(namefn, exprfn, typefn, idx, tail)
    }
  }
}

export class If extends Expr {
  readonly kind = "if"
  constructor (readonly test :Expr, readonly texp :Expr, readonly fexp :Expr) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
  //       If test texp fexp -> case idx of
  //         0 -> If (editSubExpr test) texp fexp
  //         1 -> If test (editSubExpr texp) fexp
  //         2 -> If test texp (editSubExpr fexp)
  //         _ -> expr
    return this
  }
}

export class Case extends Expr {
  readonly kind = "case"
  constructor (readonly scrut :Expr, readonly cases :Expr[]) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
  //       Case scrut cases -> case idx of
  //         0 -> Case (editSubExpr scrut) cases
  //         _ -> case modifyAt (idx-1) editSubExpr cases of
  //           Just ncases -> Case scrut ncases
  //           Nothing -> expr
    return this
  }
}

export class CaseCase extends Expr {
  readonly kind = "casecase"
  constructor (readonly pat :Expr, // TODO: this should be a pattern tree
               readonly body :Expr) { super() }

  protected editAt (namefn :(old :Name) => Name,
                    exprfn :(old :Expr) => Expr,
                    typefn :(old :Type) => Type,
                    idx :number, tail :Path) :Tree {
  //       CaseCase pat body -> case idx of
  //         0 -> CaseCase (editSubExpr pat) body
  //         1 -> CaseCase pat (editSubExpr body)
  //         _ -> expr
    return this
  }
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
