module Names where

import Prelude

newtype Name = Name String

derive newtype instance showName :: Show Name
