module Veins.Control.Monad.VExceptTSpec where

import Veins.TestPrelude
import Veins.Control.Monad.VExceptT (liftVExceptT, throwVExceptT, VExceptT (runVExceptT), evalVExceptT, catchVExceptT)
import Veins.Data.VEither (throwVEither)

data A = A deriving (Show, Eq)
data B = B deriving (Show, Eq)
data C = C deriving (Show, Eq)

spec :: Spec
spec = fdescribe "VExceptT" $ do
  describe "catchVExceptT should" $ do
    it "catch single errors" $ do
      let
        x :: VExceptT '[B] IO C
        x = do
          throwVExceptT B
        y :: VExceptT '[] IO C
        y = x `catchVExceptT` \(_::B) -> pure C
      evalVExceptT y `shouldBeM` C
    it "catch multiple errors" $ do
      let
        x :: VExceptT '[B, A] IO C
        x = do
          throwVExceptT B
        y :: VExceptT '[] IO C
        y = x `catchVExceptT` (\(_::B) -> pure C) `catchVExceptT` (\(_::A) -> pure C)
      evalVExceptT y `shouldBeM` C
  describe "bind should" $ do
    it "work as expected" $ do
      let
        d :: VExceptT '[] IO C
        d = pure C
      evalVExceptT d `shouldBeM` C
  describe "liftVExceptT" $ do
    it "should unify underlying error type with the expected error type." $ do
      let
        eb :: VExceptT '[B] IO C
        eb = throwVExceptT B
        ea :: VExceptT '[A] IO C
        ea = throwVExceptT A
        x :: VExceptT '[A,B] IO C
        x = do
          liftVExceptT eb
          liftVExceptT ea
          pure C

      runVExceptT x `shouldBeM` throwVEither B
      pure () :: IO ()
  describe "evalVExceptT should" $ do
    it "return the value of the error-free underlying VExceptT" $ do
      let
        c :: VExceptT '[] IO C
        c = pure C

      evalVExceptT c `shouldBeM` C