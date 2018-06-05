module Util where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)

-- | An infix form of `fromMaybe` with arguments flipped.
fromMaybe' :: forall a. Maybe a -> a -> a
fromMaybe' = flip fromMaybe

infixl 9 fromMaybe' as ?:
