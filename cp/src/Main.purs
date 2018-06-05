module Main where

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude

import Editor as E
import Trees as T

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (E.defEditor T.revExample) unit body
