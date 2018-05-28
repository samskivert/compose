module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Editor as E

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI E.defnEditor unit body
