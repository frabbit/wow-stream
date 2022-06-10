{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Wow.WowApp where

import qualified Data.Text as T
import Prelude
import qualified Network.WebSockets as WS
import qualified Wow.Effects.WebSocket as WSE
import Wow.Websocket (newServerState, application, broadcastSilentWhen, ServerState, applicationPoly)
import GHC.Conc (newTVarIO, readTVarIO, TVar)
import Wow.Twitter.Types (StreamEntry, loadDotEnv)

import Conduit (ConduitT, MonadIO (..))
import Data.Conduit.Combinators (iterM)
import Polysemy (Sem, Member, Embed, embedToFinal, runFinal, Final, raise)
import Wow.Effects.HttpLongPolling (HttpLongPolling, httpLongPollingToIO)
import Wow.Twitter.FilteredStream (filteredStream, filteredStreamPoly)
import Polysemy.Async (Async, async, asyncToIOFinal, await)
import Data.Function ((&))
import Control.Concurrent.Extra ( forkIO )
import Polysemy.Conc.Effect.Interrupt (Interrupt, killOnQuit)
import Polysemy.Conc (interpretInterrupt, Critical, Race, interpretCritical, interpretRace)
import Wow.Effects.WebSocket (WebSocket, webSocketToIO)

filteredStreamBroadcastPoly :: forall r . (Member HttpLongPolling r, Member (Embed IO) r) => TVar ServerState -> Sem r ()
filteredStreamBroadcastPoly var = filteredStreamPoly broadcastC
  where
  broadcastC :: _ => StreamEntry -> Sem r ()
  broadcastC s = do
      let when client = client.listening && toBool (fmap (`T.isInfixOf` s.tweet.text) client.tweetFilter)

      serverState <- liftIO $ readTVarIO var
      liftIO $ broadcastSilentWhen when s.tweet.text serverState
      pure ()
    where
      toBool (Just x) = x
      toBool Nothing = True

filteredStreamBroadcast :: _ -> IO ()
filteredStreamBroadcast var = filteredStream broadcastC
  where
  broadcastC :: (ConduitT StreamEntry StreamEntry IO ())
  broadcastC = iterM $ \s -> do
      let when client = client.listening && toBool (fmap (`T.isInfixOf` s.tweet.text) client.tweetFilter)
      serverState <- readTVarIO var
      broadcastSilentWhen when s.tweet.text serverState
      pure ()
    where
      toBool (Just x) = x
      toBool _ = False


main :: IO ()
main = do
  loadDotEnv
  state <- newTVarIO newServerState
  _ <- forkIO $ filteredStreamBroadcast state
  WS.runServer "127.0.0.1" 8130 $ application state

type AppContraints r = (WebSocket ': HttpLongPolling ': Interrupt ': Critical ':  Race ': Async ': Embed IO ': Final IO ': '[])
appToIo :: Sem (AppContraints r) a -> IO a
appToIo app = app
    & webSocketToIO appToIo
    & httpLongPollingToIO (appToIo . raise)
    & interpretInterrupt
    & interpretCritical
    & interpretRace
    & asyncToIOFinal
    & embedToFinal
    & runFinal


main' :: IO ()
main' = do
  mainPoly & appToIo

mainPoly :: ( Member WebSocket r, Member Async r, Member (Embed IO) r, Member HttpLongPolling r, Member Interrupt r) =>Sem r ()
mainPoly = do
  liftIO loadDotEnv
  state <- liftIO $ newTVarIO newServerState
  a1 <- async $ killOnQuit "arg" $ filteredStreamBroadcastPoly state
  a2 <- async $ killOnQuit "arg" $ WSE.runServer "127.0.0.1" 8130 $ applicationPoly state
  await a1
  await a2
  pure ()


