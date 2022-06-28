{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wow.Effects.ServerApiSpec where

import Control.Arrow ((>>>))
import Polysemy (Member, Sem, interpretH, pureT, run)
import Polysemy.AtomicState (AtomicState, atomicStateToState)
import Polysemy.Internal.Tactics (liftT)
import Polysemy.State (State, put, runState)
import Veins.Control.Monad.VExceptT (runVExceptT)
import Wow.Data.Client (Client, initClient)
import Wow.Data.ClientId (ClientId (..))
import Wow.Data.ServerMessage (ServerMessage (SMAcknowledge))
import Wow.Data.ServerState
  ( ServerState (..),
    addClient,
    newServerState,
  )
import Wow.Effects.ClientChannel (ClientChannel (..))
import Wow.Effects.ServerApi (ServerApi, interpretServerApi, listen)
import Wow.TestPrelude

sampleClient :: Client
sampleClient = initClient "Joe" (ClientId "abc")

interpretClientChannel :: Member (State [ServerMessage]) r => Sem (ClientChannel ': r) a -> Sem r a
interpretClientChannel = interpretH $ \case
  SendMessage _ t -> liftT $ do
    put @[ServerMessage] [t]
    pure . pure $ ()
  ReceiveMessage _ -> pureT $ error "not implemented"

interpretMocks ::
  Sem
    [ ServerApi,
      ClientChannel,
      AtomicState ServerState,
      State ServerState,
      State [ServerMessage]
    ]
    a ->
  ([ServerMessage], (ServerState, a))
interpretMocks =
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
    describe "listen should" $ do
      it "send an acknowledgement to the client" $ do
        let app = runVExceptT $ listen sampleClient
        (fst . interpretMocks $ app) `shouldBe` [SMAcknowledge "listen"] :: IO ()
      it "set the serverstate for the user to listen" $ do
        let app = runVExceptT $ listen sampleClient
        (fst . snd . interpretMocks $ app) `shouldBe` ServerState {clients = [sampleClient{listening = True}]} :: IO ()
