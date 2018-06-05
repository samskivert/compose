module Constants where

import Prelude

data Tag
 = VoidTag
 | UnitTag
 | BoolTag
 | IntTag
 | FloatTag
 | CharTag
 | StringTag
 | RawStrTag

derive instance eqTag :: Eq Tag

instance showTag :: Show Tag where
  show VoidTag = "v"
  show UnitTag = "u"
  show BoolTag = "b"
  show IntTag = "i"
  show FloatTag = "f"
  show CharTag = "c"
  show StringTag = "s"
  show RawStrTag = "r"

data Constant = Constant Tag String

derive instance eqConstant :: Eq Constant
instance showConstant :: Show Constant where
  show (Constant tag cst) = (show tag) <> "@" <> (show cst)

constUnit :: Constant
constUnit = Constant UnitTag ""

constTrue :: Constant
constTrue = Constant BoolTag "true"

constFalse :: Constant
constFalse = Constant BoolTag "false"
