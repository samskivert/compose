module Trees where

import Prelude
import Control.Alt ((<|>))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

import Names (Name(..))
import Types (Type(..))
import Constants (Constant)

-- type Records = Array Tree
-- type Fields = Array Tree
-- type CondCase = Tuple Tree Tree

data Arg = Arg Name Type

data Tree
  {- definition nodes -}
  = Def Name Tree
--  | Data Name Records
--  | Record Name Fields
--  | Field Name Type
  {- expression nodes -}
  | Lit Constant
  | Ref Name
  | App Tree Tree
  | Let Name Type Tree Tree
  | Abs Name Type Tree
  | If Tree Tree Tree
  | CaseCase Tree Tree
  | Case Tree (Array Tree)
--  Cond Array CondCase

derive instance genericTree :: Generic Tree _

instance showTree :: Show Tree where
  show tree = genericShow tree

ref :: String -> Tree
ref name = Ref (Name name)

firstEditable :: Tree -> Maybe Tree
firstEditable tree = case tree of
  Def _ _ -> Just tree
  Lit _ -> Just tree
  Ref _ -> Just tree
  App fun arg -> (firstEditable fun) <|> (firstEditable arg)
  Let _ _ _ _ -> Just tree -- editing the let name
  Abs _ _ _ -> Just tree -- editing the arg name
  If test tx fx -> (firstEditable test) <|> (firstEditable tx) <|> (firstEditable fx)
  CaseCase pat body -> (firstEditable pat) <|> (firstEditable body)
  Case scrut cases -> (firstEditable scrut) <|> (foldl (\b a -> b <|> (firstEditable a)) Nothing cases)

-- reverse as :List A -> List A =
--   let revacc as acc = case as of
--     Nil -> acc
--     Cons h t -> revacc t h :: acc
--   in revacc as Nil

revExample :: Tree
revExample =
   Def (Name "reverse") $
   Abs (Name "as") tpListA $
   Let (Name "revacc") Unknown revAccDef $
   App (ref "revacc") (App (ref "as") (ref "Nil"))
 where
   tpListA = Apply (Ctor (Name "List")) (Var (Name "A"))
   revAccDef =
     Abs (Name "as") tpListA $
     Abs (Name "acc") tpListA $
     Case (ref "as") [
       CaseCase (ref "Nil") (ref "acc"),
       CaseCase (App (App (ref "Cons") (ref "h")) (ref "t"))
                (App (App (ref "revacc") (ref "t"))
                     (App (App (ref "Cons") (ref "h")) (ref "acc")))
     ]
