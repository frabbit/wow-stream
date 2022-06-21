{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Wow.Websocket.TestClient where
import Prelude

import Control.Concurrent.STM (TMVar, newEmptyTMVarIO, atomically, takeTMVar, isEmptyTMVar, STM, putTMVar)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Data.Text (Text)
import Debug.Trace (traceShowM)
import Control.Exception (Exception (fromException), throwIO)
import Control.Concurrent.Async (async, waitCatch, Async)
import GHC.Natural (Natural, naturalToInteger)
import Network.WebSockets (ConnectionException (CloseRequest))
import Control.Monad (forever)

type Message = Text

type OnReceive = ClientId -> Message -> IO ()

type ClientId = Text

type Command = Text

data ClientConfig = ClientConfig {
  send :: TMVar (Maybe Command),
  onReceive :: OnReceive,
  port :: Natural,
  clientId :: ClientId
}

data ClientAction
  = CAReceive Message
  | CASend (Maybe Command)


startTestClient :: ClientId -> Natural -> OnReceive -> IO
     (Maybe Command -> IO (), Async ())
startTestClient clientId port onReceive = do
  send <- newEmptyTMVarIO
  let config = ClientConfig {
    clientId, send, onReceive, port
  }
  fiber <- async $ exec config
  let
    send' v = do
      atomically $ putTMVar send v
  pure (send', fiber)
  where
    exec :: ClientConfig -> IO ()
    exec config = withSocketsDo $ WS.runClient "127.0.0.1" (fromInteger $ naturalToInteger config.port) "/" (testClient config)

waitIsEmptyTMVar :: TMVar a -> STM ()
waitIsEmptyTMVar x= do
  res <- isEmptyTMVar x
  if res then pure () else waitIsEmptyTMVar x

testClient :: ClientConfig -> WS.ClientApp ()
testClient ClientConfig { send, onReceive, clientId } conn = do

  fiber <- async $ forever $ do
    (msg::Text) <- WS.receiveData conn
    onReceive clientId msg

  let loop = do
        cmd <- atomically $ takeTMVar send
        case cmd of
          Nothing -> do
            pure ()
          Just cmd' -> WS.sendTextData conn cmd' >> loop
  loop
  WS.sendClose conn ("Bye\n"::Text)
  x <- waitCatch fiber
  case x of
    Left e -> do
      let (connErr::Maybe ConnectionException) = fromException e
      case connErr of
        Just CloseRequest {} -> pure ()
        _ -> do
          traceShowM e
          throwIO e

    Right _ -> pure ()



