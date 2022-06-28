{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wow.Effects.ServerApiSpec where

import Control.Arrow ((>>>))
import Polysemy (Member, Sem, interpretH, pureT, run)
import Polysemy.AtomicState (AtomicState, atomicStateToState)
import Polysemy.Internal.Tactics (liftT)
import Polysemy.State (State, put, runState)
import Test.Hspec (context)
import Veins.Control.Monad.VExceptT (runVExceptT)
import Wow.Data.Client (Client, initClient)
import Wow.Data.ClientId (ClientId (..))
import Wow.Data.ServerMessage (ServerMessage (SMAcknowledge, SMClients))
import Wow.Data.ServerState
  ( ServerState (..),
    addClient,
    newServerState,
  )
import Wow.Effects.ClientChannel (ClientChannel (..))
import Wow.Effects.ServerApi (ServerApi, filter, interpretServerApi, listen, unlisten, listClients)
import Wow.TestPrelude hiding (filter)

sampleClient :: Client
sampleClient = initClient "Joe" (ClientId "abc")

interpretClientChannel :: Member (State [ServerMessage]) r => Sem (ClientChannel ': r) a -> Sem r a
interpretClientChannel = interpretH $ \case
  SendMessage _ t -> liftT $ do
    put @[ServerMessage] [t]
    pure . pure $ ()
  ReceiveMessage _ -> pureT $ error "not implemented"

interpretApp ::
  Sem
    [ ServerApi,
      ClientChannel,
      AtomicState ServerState,
      State ServerState,
      State [ServerMessage]
    ]
    a ->
  ([ServerMessage], (ServerState, a))
interpretApp =
  interpretServerApi
    >>> interpretClientChannel
    >>> atomicStateToState
    >>> runState serverState
    >>> runState []
    >>> run
  where
    serverState = addClient sampleClient newServerState

spec :: Spec
spec = describe "ServerApi" $ do
  describe "interpretClientChannel" $ do
    context "when interpreting listen should" $ do
      let app = runVExceptT $ listen sampleClient

      it "send an acknowledgement to the client" $ do
        (fst . interpretApp $ app) `shouldBe` [SMAcknowledge "listen"] :: IO ()

      it "enable listening for the user" $ do
        (fst . snd . interpretApp $ app) `shouldBe` ServerState {clients = [sampleClient{listening = True}]} :: IO ()

    context "when interpreting unlisten should" $ do
      let app = runVExceptT $ unlisten sampleClient

      it "send an acknowledgement to the client" $ do
        (fst . interpretApp $ app) `shouldBe` [SMAcknowledge "unlisten"] :: IO ()

      it "disable listening for the user" $ do
        (fst . snd . interpretApp $ app) `shouldBe` ServerState {clients = [sampleClient{listening = False}]} :: IO ()

    context "when interpreting filter should" $ do
      let app = runVExceptT $ filter "abc" sampleClient

      it "send an acknowledgement to the client" $ do
        (fst . interpretApp $ app) `shouldBe` [SMAcknowledge "filter"] :: IO ()

      it "disable setting the filter for the user" $ do
        (fst . snd . interpretApp $ app) `shouldBe` ServerState {clients = [sampleClient{tweetFilter = Just "abc"}]} :: IO ()

    context "when interpreting listClients should" $ do
      let app = runVExceptT $ listClients sampleClient

      it "send the list to the client" $ do
        (fst . interpretApp $ app) `shouldBe` [SMClients ["Joe"]] :: IO ()
