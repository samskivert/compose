module Names where

import Prelude

newtype Name = Name String

toString :: Name -> String
toString (Name name) = name

derive newtype instance eqName :: Eq Name
derive newtype instance showName :: Show Name
