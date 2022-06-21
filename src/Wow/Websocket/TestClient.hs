{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StandaloneDeriving #-}
module Wow.Websocket.TestClient where
import Prelude

import Control.Concurrent.STM (TMVar, newEmptyTMVarIO, atomically, takeTMVar, isEmptyTMVar, STM)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Data.Text (Text)
import Debug.Trace (traceShowM)
import Control.Exception (Exception (fromException), throwIO)
import Control.Concurrent.Async (async, waitCatch, cancelWith, Async)
import GHC.Natural (Natural, naturalToInteger)
import Network.WebSockets (ConnectionException (CloseRequest))

data ExpectationError = ExpectationError Text
  deriving (Show, Eq)

instance Exception ExpectationError

data Stopped = Stopped deriving (Show, Eq)

instance Exception Stopped


type OnReceive = ClientId -> Text -> IO ()

type ClientId = Text

data ClientConfig = ClientConfig {
  send :: TMVar (Maybe Text),
  onReceive :: OnReceive,
  port :: Natural,
  clientId :: ClientId
}

data ClientAction
  = CAReceive Text
  | CASend (Maybe Text)

startTestClient :: ClientId -> Natural -> OnReceive -> IO
     (TMVar (Maybe Text), Async ())
startTestClient clientId port onReceive = do
  send <- newEmptyTMVarIO
  let config = ClientConfig {
    clientId, send, onReceive, port
  }
  fiber <- async $ exec config
  pure (send, fiber)
  where
    exec :: ClientConfig -> IO ()
    exec config = withSocketsDo $ WS.runClient "127.0.0.1" (fromInteger $ naturalToInteger config.port) "/" (testClient config)

waitIsEmptyTMVar :: TMVar a -> STM ()
waitIsEmptyTMVar x= do
  res <- isEmptyTMVar x
  if res then pure () else waitIsEmptyTMVar x

testClient :: ClientConfig -> WS.ClientApp ()
testClient ClientConfig { send, onReceive, clientId } conn = do
  let
    loopReceive :: IO ()
    loopReceive = do
      (msg::Text) <- WS.receiveData conn
      onReceive clientId msg
      loopReceive

  fiber <- async loopReceive

  let loop = do
        cmd <- atomically $ takeTMVar send
        case cmd of
          Nothing -> do
            --WS.sendClose conn ("Bye\n"::Text)
            cancelWith fiber Stopped
            pure ()
          Just cmd' -> WS.sendTextData conn cmd' >> loop
  loop
  x <- waitCatch fiber
  case x of
    Left e -> do
      let (x1::Maybe Stopped) = fromException e
      let (x2::Maybe ConnectionException) = fromException e
      case (x1, x2) of
        (Just _, _) -> pure ()
        (_, Just CloseRequest {}) -> pure ()
        _ -> do
          traceShowM e
          throwIO e

    Right _ -> pure ()
  traceShowM "done"



