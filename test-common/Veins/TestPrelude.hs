{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Veins.TestPrelude (module M, shouldUnify, shouldBeM) where

import Prelude as M

import Data.Function as M ((&))

import Test.Hspec as M (Spec, fdescribe, it, describe, fit, xit, xdescribe, shouldBe, shouldSatisfy)

import Test.QuickCheck as M (property)
import Control.Monad.IO.Class as M (MonadIO (liftIO))

shouldUnify :: forall x y. (y ~ x) => y -> IO ()
shouldUnify _ = pure ()

shouldBeM :: (Show x, Eq x, MonadIO m) => m x -> x -> m ()
shouldBeM x w = do
  m <- x
  liftIO $ m `M.shouldBe` w