module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Test.Assert (assert)

import Trees as T

funApp :: T.Tree
funApp = T.App (T.ref "pants") (T.ref "legs")

main :: Effect Unit
main = do
  logShow (T.firstEditable funApp)
