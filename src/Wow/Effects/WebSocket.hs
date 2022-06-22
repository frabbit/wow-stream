{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Wow.Effects.WebSocket where

import Prelude

import qualified Network.WebSockets as WS
import Polysemy (makeSem, Member, Embed, Sem, interpretH, runT, embed, bindT, getInitialStateT, pureT)
import Data.Functor (($>), (<&>))
import Control.Monad (void)
import Data.Text (Text)
import Debug.Trace (traceShowM)
import Control.Monad.IO.Class (liftIO)
import Network.WebSockets (ConnectionException)
import Control.Exception (catch)

data WebSocket m a where
  WithPingThread :: WS.Connection -> Int -> m () -> m a -> WebSocket m a
  RunServer :: String -> Int -> (WS.PendingConnection -> m ()) -> WebSocket m ()
  ReceiveData :: (WS.WebSocketsData a) => WS.Connection -> WebSocket m (Either ConnectionException a)
  SendTextData :: WS.Connection -> Text -> WebSocket m ()
  AcceptRequest :: WS.PendingConnection -> WebSocket m WS.Connection

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
    let
      app'' f = do
        traceShowM ("before app\n\n"::Text)
        app' f
        traceShowM ("done ...\n\n"::Text)
    x <- liftIO $ do
      traceShowM ("run server"::Text)
      WS.runServer address port app''
      traceShowM ("run server done..."::Text)

    pureT x
  ReceiveData conn -> do
    pureT =<< (embed $
      (WS.receiveData conn <&> Right) `catch` \(e::ConnectionException) -> do
        pure $ Left e
      )
  SendTextData conn txt -> do
    pureT =<< (embed $ WS.sendTextData conn txt)
  AcceptRequest pendingConn -> do
    pureT =<< (embed $ WS.acceptRequest pendingConn)


