module Trees where

import Control.Alt ((<|>))
import Data.Array (head, snoc, uncons, modifyAt)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect, foreachE)
import Effect.Class.Console (log)
import Prelude

import Names (Name(..))
import Types (Type(..))
import Constants (Constant, constTrue)

-- ----------------
-- Definition model
-- ----------------

type FieldDef = {name :: Name, tpe :: Type}
type RecordDef = {name :: Name, fields :: Array FieldDef}

-- | Definition AST.
data Def
  = Term Name Expr
  | Union Name (Array RecordDef)
  | Record RecordDef

derive instance eqDef :: Eq Def
derive instance genericDef :: Generic Def _
instance showDef :: Show Def where
  show def = genericShow def

-- ----------------
-- Expression model
-- ----------------

-- TODO: factor `type Binding = {name :: Name, tpe :: Type, value :: Expr}` out of Let/Abs?
-- TODO: create doc AST, add Doc node to every node that defines a name (Def, Let, Abs, etc.)

-- | Expression AST.
data Expr
  = Lit Constant
  | Ref Name
  | Hole Type
  | App Expr Expr
  | Let Name Type Expr Expr
  | Abs Name Type Expr
  | If Expr Expr Expr
  | Case Expr (Array Expr)
  | CaseCase Expr Expr
--   | Cond Array CondCase
--   | CondCase Expr Expr

derive instance eqExpr :: Eq Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show expr = genericShow expr

-- ----------
-- Path model
-- ----------

-- A path identifies a point in an AST, generally used for editing. The editing cursor uses paths to
-- track the insertion point. The selection is a pair of paths that track the start and end of the
-- selection.

-- | Identifies a path from some root node to a field of some target node. Each path component is
-- | the index of the field in the current node. If a path has additional components that field must
-- | represent another AST node, but the final component of a path may reference a terminal field
-- | like a `Name` or `Constant`.
-- |
-- | Note that a path will generally start in a `Def` node and may traverse into `Expr` nodes en
-- | route to its terminal node (which may itself be a `Def` node, an `Expr` node, a `Name`, etc.).
-- | Thus we cannot, in general, obtain the "thing" to which a path points as there is no single
-- | type that describes it.
type Path = Array Int

emptyPath :: Path
emptyPath = []

extendPath :: Path -> Int -> Path
extendPath path idx = snoc path idx

-- ----------
-- Edit model
-- ----------

-- | Abstracts over editable trees: `Def` and `Expr` (and maybe someday `Type`?).
class Editable a where
  -- | Replaces the expression (`expr`) at `path` in `tree` with the result of `exprfn expr`.
  editTree :: Path -> (Name -> Name) -> (Expr -> Expr) -> (Type -> Type) -> a -> a
  -- | Replaces the name (`name`) at `path` in `tree` with the result of `namefn name`.
  -- editName :: Path -> (Name -> Name) -> a -> a
  -- | Replaces the type (`type`) at `path` in `tree` with the result of `typefn type`.
  -- editType :: Path -> (Type -> Type) -> a -> a

-- A tree edit is simply a function from tree to tree, but we provide functions to construct edits
-- out of paths and subtrees or edit operations to existing tree nodes.

editName :: forall a. Editable a => Path -> (Name -> Name) -> a -> a
editName path namefn tree = editTree path namefn identity identity tree

editExpr :: forall a. Editable a => Path -> (Expr -> Expr) -> a -> a
editExpr path exprfn tree = editTree path identity exprfn identity tree

editType :: forall a. Editable a => Path -> (Type -> Type) -> a -> a
editType path typefn tree = editTree path identity identity typefn tree

instance editableExpr :: Editable Expr where
  editTree path namefn exprfn typefn expr = case uncons path of
    Nothing -> exprfn expr
    Just {head: idx, tail} ->
      let editSubExpr = editTree tail namefn exprfn typefn
      in case expr of
        Lit _ -> expr
        Ref name -> Ref (namefn name)
        Hole tpe -> Hole (typefn tpe)
        App fun arg -> case idx of
          0 -> App (editSubExpr fun) arg
          1 -> App fun (editSubExpr arg)
          _ -> expr
        Let name tpe value body -> case idx of
          0 -> Let (namefn name) tpe value body
          1 -> Let name (typefn tpe) value body
          2 -> Let name tpe (editSubExpr value) body
          3 -> Let name tpe value (editSubExpr body)
          _ -> expr
        Abs name tpe value -> case idx of
          0 -> Abs (namefn name) tpe value
          1 -> Abs name (typefn tpe) value
          2 -> Abs name tpe (editSubExpr value)
          _ -> expr
        If test texp fexp -> case idx of
          0 -> If (editSubExpr test) texp fexp
          1 -> If test (editSubExpr texp) fexp
          2 -> If test texp (editSubExpr fexp)
          _ -> expr
        Case scrut cases -> case idx of
          0 -> Case (editSubExpr scrut) cases
          _ -> case modifyAt (idx-1) editSubExpr cases of
            Just ncases -> Case scrut ncases
            Nothing -> expr
        CaseCase pat body -> case idx of
          0 -> CaseCase (editSubExpr pat) body
          1 -> CaseCase pat (editSubExpr body)
          _ -> expr

instance editableDef :: Editable Def where
  editTree path namefn exprfn typefn def =  case uncons path of
    Nothing -> def
    Just {head: idx, tail} -> case def of
      Term name expr -> case idx of
        0 -> Term (namefn name) expr
        -- TODO: 1 -> Term name (typefn tpe) expr
        2 -> Term name (editTree tail namefn exprfn typefn expr)
        _ -> def
      Union name records -> case idx of
        0 -> Union (namefn name) records
        _ -> case modifyAt (idx-1) (editRecord tail) records of
          Just nrecords -> Union name nrecords
          Nothing -> def
      Record record -> Record (editRecord path record)
   where
    editRecord path {name, fields} = case uncons path of
      Nothing -> {name, fields}
      Just {head: idx, tail} -> case idx of
        0 -> {name: namefn name, fields}
        _ -> case modifyAt (idx-1) (editField tail) fields of
          Just nfields -> {name: name, fields: nfields}
          Nothing -> {name, fields}
    editField path {name, tpe} = case uncons path of
      Nothing -> {name, tpe}
      Just {head: idx, tail} -> case idx of
        0 -> {name: namefn name, tpe}
        1 -> {name, tpe: typefn tpe}
        _ -> {name, tpe}

follow :: forall a. Path -> (Path -> Int -> a) -> a
follow path idxfn = case uncons path of
  Just {head, tail} -> idxfn tail head
  Nothing -> idxfn [] 0 -- TODO: report failure for malformed path?

firstEditable :: Expr -> Maybe Expr
firstEditable expr = case expr of
  Lit _ -> Just expr
  Ref _ -> Just expr
  Hole _ -> Just expr
  App fun arg -> (firstEditable fun) <|> (firstEditable arg)
  Let _ _ _ _ -> Just expr -- editing the let name
  Abs _ _ _ -> Just expr -- editing the arg name
  If test tx fx -> (firstEditable test) <|> (firstEditable tx) <|> (firstEditable fx)
  Case scrut cases -> (firstEditable scrut) <|>
    (foldl (\b a -> b <|> (firstEditable a)) Nothing cases)
  CaseCase pat body -> (firstEditable pat) <|> (firstEditable body)

-- --------------
-- Debug printing
-- --------------

debugShowExpr :: String -> Expr -> Array String
debugShowExpr indent = case _ of
  Lit const -> [ pre "Lit" <> show const ]
  Ref name -> [ pre "Ref" <> show name ]
  Hole tpe -> [ pre "Hole" <> "? :: " <> show tpe ]
  App fun arg -> [ pre "App" ] <> showSub fun <> showSub arg
  Let name tpe value body ->
    [ pre "Let" <> show name <> " :: " <> show tpe ] <> showSub value <> showSub body
  Abs name tpe value -> [ pre "Abs" <> show name <> " :: " <> show tpe ] <> showSub value
  If test texp fexp -> [ pre "If" ] <> showSub test <> showSub texp <> showSub fexp
  Case scrut cases -> [ pre "Case" ] <> showSub scrut <> (cases >>= showSub)
  CaseCase pat body -> [ pre "CaseCase" ] <> showSub pat <> showSub body
 where
  pre id = indent <> id <> ": "
  showSub expr = debugShowExpr (indent <> " ") expr

logExpr :: Expr -> Effect Unit
logExpr expr = foreachE (debugShowExpr "" expr) log

debugShowDef :: String -> Def -> Array String
debugShowDef indent = case _ of
  Term name expr -> [ pre "Term" <> show name] <> debugShowExpr (indent <> " ") expr
  Union name records -> [ pre "Union" <> show name ] <> (records >>= showRecord)
  Record rdef -> showRecord rdef
 where
  pre id = indent <> id <> ": "
  showField {name, tpe} = indent <> " Field: " <> show name <> " :: " <> show tpe
  showRecord {name, fields} = [ indent <> " Record: " <> show name ] <> (map showField fields)

logDef :: Def -> Effect Unit
logDef def = foreachE (debugShowDef "" def) log

-- ----------
-- Test trees
-- ----------

ref :: String -> Expr
ref name = Ref (Name name)

-- let foo = true
letExample :: Def
letExample =
  Term (Name "foo") $
  Lit constTrue

-- reverse as :List A -> List A =
--   let revacc as acc = case as of
--     Nil -> acc
--     Cons h t -> revacc t h :: acc
--   in revacc as Nil
revExample :: Def
revExample =
  Term (Name "reverse") $
  Abs (Name "as") tpListA $
  Let (Name "revacc") Unknown revAccDef $
  App (ref "revacc") (App (ref "as") (ref "Nil"))
 where
  tpListA = Apply (Ctor (Name "List")) (Var (Name "A"))
  revAccDef =
    Abs (Name "as") tpListA $
    Abs (Name "acc") tpListA $
    Case (ref "as") [
      CaseCase (ref "Nil") (Hole Unknown), -- ref "acc"
      CaseCase (App (App (ref "Cons") (ref "h")) (ref "t"))
               (App (App (ref "revacc") (ref "t"))
                    (App (App (ref "Cons") (ref "h")) (ref "acc")))
    ]
