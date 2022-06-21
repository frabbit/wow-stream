{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Veins.Data.VariantSpec where

import Veins.TestPrelude

import Veins.Data.Variant

data A = A deriving (Show, Eq)
data B = B deriving (Show, Eq)
data C = C deriving (Show, Eq)

spec :: Spec
spec = describe "Variant" $ do
  describe "inject should" $ do
    it "create a Variant from a value" $ do
      inject B & shouldUnify @(Variant '[B])
      inject B & shouldUnify @(Variant '[B])
      inject B & shouldUnify @(Variant '[A, B])
      inject B & shouldUnify @(Variant '[B, A])
      inject B & shouldUnify @(Variant '[A, B, C])
  describe "lift should" $ do
    it "lift a smaller Variant into a bigger Variant" $ do
      let
        b :: Variant '[B]
        b = inject B
        ab :: Variant '[A, B]
        ab = inject B
      liftVariant b & shouldUnify @(Variant '[A, B, C])
      liftVariant b & shouldUnify @(Variant '[B, C])
      liftVariant b & shouldUnify @(Variant '[C, B])
      liftVariant b & shouldUnify @(Variant '[B])

      liftVariant ab & shouldUnify @(Variant '[A,B])
      liftVariant ab & shouldUnify @(Variant '[B,A])
      liftVariant ab & shouldUnify @(Variant '[A,B,C])
      liftVariant ab & shouldUnify @(Variant '[B,C,A])
      liftVariant ab & shouldUnify @(Variant '[C,B,A])

  describe "project should" $ do
    it "convert a variant to a maybe" $ do
      let
        b :: Variant '[B]
        b = inject B
        ab :: Variant '[A, B]
        ab = inject B
        ba :: Variant '[B, A]
        ba = inject B
      project @B b `shouldBe` Just B
      project @B ab `shouldBe` Just B
      project @A ab `shouldBe` Nothing
      project @B ba `shouldBe` Just B
      project @A ba `shouldBe` Nothing

      pure () :: IO ()
  describe "remove should" $ do
    it "remove an option from the given variant as Left or the value itself as Right" $ do
      let
        ab :: Variant '[A, B]
        ab = inject B
        ba :: Variant '[B, A]
        ba = inject B
        abc :: Variant '[A, B, C]
        abc = inject B
        cba :: Variant '[C, B, A]
        cba = inject B
      remove @B ab & shouldUnify @(Either (Variant '[A]) B)
      remove @A ab & shouldUnify @(Either (Variant '[B]) A)
      remove @B ba & shouldUnify @(Either (Variant '[A]) B)
      remove @A ba & shouldUnify @(Either (Variant '[B]) A)

      remove @B abc & shouldUnify @(Either (Variant '[A,C]) B)
      remove @B cba & shouldUnify @(Either (Variant '[C,A]) B)

      remove @A ab `shouldBe` Left (inject B)
      remove @B ab `shouldBe` Right B


