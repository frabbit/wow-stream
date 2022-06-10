{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Wow.Effects.WebSocket where

import Prelude

import qualified Network.WebSockets as WS
import Polysemy (makeSem, Member, Embed, Sem, interpretH, runT, embed, bindT, getInitialStateT, pureT)
import Data.Functor (($>))
import Control.Monad (void)

data WebSocket m a where
  WithPingThread :: WS.Connection -> Int -> m () -> m a -> WebSocket m a
  RunServer :: String -> Int -> (WS.PendingConnection -> m ()) -> WebSocket m ()

makeSem ''WebSocket

webSocketToIO :: forall r a . (Member (Embed IO) r) => (forall x . Sem (WebSocket ': r) x -> IO x) -> Sem (WebSocket ': r) a -> Sem r a
webSocketToIO nt = interpretH $ \case
  WithPingThread conn interval pingAction appAction -> do
    pingAction' <- runT pingAction
    appAction' <- runT appAction
    embed $ do
      let pa = nt pingAction' $> ()
      let aa = nt appAction'
      WS.withPingThread conn interval pa aa
  RunServer address port app -> do
    appF <- bindT app
    is <- getInitialStateT
    let app' pc = void . nt . appF $ pc <$ is
    o <- embed $ do
      WS.runServer address port app'
    pureT o



