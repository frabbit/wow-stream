{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Wow.Effects.Server where

import Wow.Prelude
import Polysemy (Sem, reinterpretH, Member, bindT, getInitialStateT, subsume, embed, Embed, makeSem)
import Wow.Effects.WebSocket (WebSocket, withPingThread, runServer, acceptRequest)
import Polysemy.Internal.Tactics (liftT)
import Network.WebSockets (Connection)
import Wow.Data.ClientId (ClientId, newClientId)
import qualified Data.Map.Strict as M
import Polysemy.AtomicState (AtomicState, atomicModify')
import GHC.Natural (naturalToInteger, Natural)
import Control.Monad (void)
import Debug.Trace (traceShowM)

type ClientLookup = M.Map ClientId Connection

data Server m a where
  Start :: (ClientId -> m a) -> Server m ()

makeSem ''Server

interpretServer :: forall r a . (Member (Embed IO) r, Member (AtomicState ClientLookup) r) => Natural -> Sem (Server ': r) a -> Sem (WebSocket ': r) a
interpretServer port = reinterpretH $ \case
  Start callback -> do
    cb <- bindT callback
    is <- getInitialStateT
    liftT $ runServer "127.0.0.1" (fromInteger $ naturalToInteger port) $ \pending -> do

      conn <- acceptRequest pending
      let
        cleanup = do
          traceShowM "Cleanup"
          pure ()
      withPingThread conn 30 cleanup $ do
        clientId <- embed newClientId
        atomicModify' (M.insert clientId conn)
        void $ subsume . (interpretServer port) $ cb (clientId <$ is)
        traceShowM "aha"
      traceShowM "connection lost"
