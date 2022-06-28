module Wow.Effects.FinallySpec where

import Wow.TestPrelude
import Wow.Effects.Finally (Finally, finally, finallyPure)
import Polysemy.State (State, modify, runState)
import Polysemy (Sem, run)

spec :: Spec
spec = fdescribe "Finally" $ do
  describe "interpretPure should" $ do
    it "work as expected" $ do
      let
        f :: Sem [Finally, State [String]] ()
        f = finally (modify @[String] ("first" :)) (modify @[String] ("second" :))

      let res = fst $ f & finallyPure & runState [] & run
      res `shouldBe` ["second", "first"]
