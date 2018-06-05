module Test.Main where

import Control.Monad.State as CMS
import Data.Array((..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Class (liftEffect)
import Prelude
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import Format as F
import Markup as M
import Names (Name(..))
import Trees as T
import Types as TP

testEdit = suite "test edit" do
  test "edit expr: app" do
    let pants = T.ref "pants"
    let legs = T.ref "legs"
    let orig = T.App pants legs
    Assert.equal (T.App (T.ref "socks") legs) $ T.editExpr [0] (const $ T.ref "socks") orig
    Assert.equal (T.App pants (T.ref "arms")) $ T.editExpr [1] (const $ T.ref "arms") orig
  test "edit expr: let" do
    let pants = Name "pants"
    let slacks = T.ref "slacks"
    let outfit = T.ref "outfit"
    let orig = T.Let pants TP.Unknown slacks outfit
    Assert.equal (T.Let (Name "bottoms") TP.Unknown slacks outfit) $
      T.editName [0] (const $ Name "bottoms") orig
    Assert.equal (T.Let pants TP.Unknown (T.ref "jeans") outfit) $
      T.editExpr [2] (const $ T.ref "jeans") orig
    Assert.equal (T.Let pants TP.Unknown slacks (T.ref "suitcase")) $
      T.editExpr [3] (const $ T.ref "suitcase") orig

main :: Effect Unit
main = runTest do
  testEdit
