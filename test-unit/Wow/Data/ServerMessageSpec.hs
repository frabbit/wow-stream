{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Wow.Data.ServerMessageSpec where

import Control.Monad (forM_)
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT (runStateT), gets, modify, runState)
import Data.Data (Data (dataTypeOf, toConstr), Proxy (Proxy), dataTypeConstrs)
import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Proxied (dataTypeOfProxied)
import Data.Proxy (Proxy)
import qualified Data.Sequence as Map
import Debug.Trace (traceShowM)
import Test.QuickCheck (arbitrary, generate, sample, Arbitrary)
import Wow.Data.Command (Command (..), parseCommand)
import Wow.Data.ServerMessage (ServerMessage (SMAcknowledge, SMSimpleText), parseServerMessage, toText)
import Wow.TestPrelude

shouldGenerateAllConstructorsOfADT :: forall x . (Arbitrary x, Data x) => Proxy x -> IO ()
shouldGenerateAllConstructorsOfADT _ = do
  let initialMap =
        M.fromList
          . fmap (\c -> (show c, 0 :: Int))
          . dataTypeConstrs
          . dataTypeOfProxied
          $ (Proxy :: Proxy x)

  (_, s) <- flip runStateT initialMap $ do
    forM_ (replicate 100 ()) $ \_ -> do
      (sm :: x) <- lift $ generate arbitrary
      let c = toConstr sm
      modify (M.update (\v -> Just (v + 1)) (show c))

  M.elems s `shouldSatisfy` all (> 0)

spec :: Spec
spec = fdescribe "ServerMessage" $ do
  describe "parseServerMessage should" $ do
    it "parse valid acknowledgements successfully" $ do
      let name = "Something"
      parseServerMessage (":acknowledge " <> name) `shouldBe` Right (SMAcknowledge name)
    it "parse unknown commands as simpleText as fallback" $ do
      let cmd = "whatever"
      parseServerMessage cmd `shouldBe` Right (SMSimpleText cmd)
  describe "parse/toText should" $ do
    it "encode and decode server message to text and back" $
      property $ \msg ->
        parseServerMessage (toText msg) `shouldBe` Right msg

  describe "arbitrary" $ do
    it "should generate all constructors" $
      shouldGenerateAllConstructorsOfADT (Proxy::Proxy ServerMessage)