module Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Constants (Constant, Tag)
import Names (Name)

data Type
  = Unknown
  | Const Constant
  | Data Tag Int
  | Arrow Type Type
  | Ctor Name
  | Var Name
  | Apply Type Type
-- | Array Type
-- | Record Name Params Fields
-- | Field Name Type
-- | Union Params Cases
-- | Interface Name Params Methods
-- | Method Name Type

derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show tpe = genericShow tpe
