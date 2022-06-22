{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Wow.Data.ServerMessageSpec where

import Control.Monad (forM_)
import Control.Monad.State (MonadTrans (lift), StateT (runStateT), modify)
import Data.Data (Data (toConstr), Proxy (Proxy), dataTypeConstrs)
import qualified Data.Map as M
import Data.Proxied (dataTypeOfProxied)
import Test.QuickCheck (arbitrary, generate, Arbitrary)
import Wow.Data.ServerMessage (ServerMessage (SMAcknowledge), parseServerMessage, toText, Error)
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
  describe "parse/toText should" $ do
    it "encode and decode server message to text and back" $
      property $ \msg ->
        parseServerMessage (toText msg) `shouldBe` Right msg

  describe "arbitrary" $ do
    it "should generate all constructors" $
      shouldGenerateAllConstructorsOfADT (Proxy::Proxy ServerMessage)
  describe "Error.arbitrary" $ do
    it "should generate all constructors" $
      shouldGenerateAllConstructorsOfADT (Proxy::Proxy Error)
