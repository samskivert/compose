module Trees where

import Prelude
import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Array (snoc)

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

-- reverse as :List A -> List A =
--   let revacc as acc = case as of
--     Nil -> acc
--     Cons h t -> revacc t h :: acc
--   in revacc as Nil

ref :: String -> Expr
ref name = Ref (Name name)

letExample :: Def
letExample =
  Term (Name "foo") $
  Lit constTrue

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
