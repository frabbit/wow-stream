{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Veins.Data.Variant.EitherUtilsSpec where

import Veins.TestPrelude

import Veins.Data.Variant ( inject, Variant )
import Veins.Data.Variant.EitherUtil (catchEitherVariant)

data A = A deriving (Show, Eq)
data B = B deriving (Show, Eq)
data C = C deriving (Show, Eq)

spec :: Spec
spec = describe "EitherUtils" $ do
  describe "catchErrorV" $ do
    it "should catch errors" $ do
      let
        b :: Variant '[A,B]
        b = inject B
      (Left b `catchEitherVariant` (\(_::B) -> Right C)) & shouldUnify @(Either (Variant '[A]) C)
      (Left b `catchEitherVariant` (\(_::A) -> Right C)) & shouldUnify @(Either (Variant '[B]) C)

      (Left b `catchEitherVariant` (\(_::B) -> Right C)) `shouldBe` Right C
      (Left b `catchEitherVariant` (\(_::A) -> Right C)) `shouldBe` Left (inject B)
