{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StandaloneDeriving #-}
module Wow.Websocket.TestClient where
import Prelude

import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO, Chan, threadDelay)
import Control.Concurrent.STM (TMVar, tryReadTMVar, readTMVar, tryTakeTMVar, newTMVarIO, newEmptyTMVarIO, putTMVar, atomically, takeTMVar, isEmptyTMVar, STM)
import Control.Monad (forever, unless)



import Network.Socket (withSocketsDo, AddrInfoFlag (AI_ADDRCONFIG))

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import System.Console.Haskeline as HL
import Control.Monad.Error (MonadError(throwError))
import Debug.Trace (traceM, traceShow, traceShowM)
import Test.Hspec (shouldBe)
import Control.Exception (Exception (fromException), throw, throwIO, SomeException (SomeException))
import Control.Concurrent.Async (async, cancel, wait, waitCatch, cancelWith, Async)
import Data.Maybe (isJust)
import System.Posix (sleep)
import GHC.Natural (Natural, naturalToInteger)

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
            cancelWith fiber Stopped
            pure ()
          Just cmd' -> WS.sendTextData conn cmd' >> loop
  loop
  x <- waitCatch fiber
  case x of
    Left e -> do
      let (x1::Maybe Stopped) = fromException e
      if isJust x1 then do
        pure ()
      else do
        traceShowM e
        throwIO e

    Right _ -> pure ()


