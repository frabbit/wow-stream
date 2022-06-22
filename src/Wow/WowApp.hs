{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# HLINT ignore "Use newTVarIO" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Wow.WowApp where


import Debug.Trace (traceShowM)
import qualified Data.Text as T
import Wow.Prelude
import Wow.Websocket (newServerState, broadcastSilentWhen, ServerState, handleClient)
import GHC.Conc (TVar, readTVar)
import Wow.Twitter.Types (StreamEntry)

import Polysemy (Sem, Member, Embed, embedToFinal, runFinal, Final, raise_, subsume, Members)
import Wow.Effects.HttpLongPolling (HttpLongPolling, httpLongPollingToIO)
import Wow.Effects.Env (Env, envToIo)
import Polysemy.Async (Async, asyncToIOFinal, sequenceConcurrently)
import Polysemy.Conc.Effect.Interrupt (Interrupt, killOnQuit, unregister)
import Polysemy.Conc (Critical, Race, interpretCritical, interpretRace, interpretInterruptOnce)
import Wow.Effects.WebSocket (WebSocket, webSocketToIO)
import Wow.Effects.Finally (Finally, finallyToIo)
import Wow.Effects.DotEnv (DotEnv, loadDotEnv, dotEnvToIo)
import Wow.Effects.STM (STM, stmToIo, atomically)
import GHC.Conc.Sync (newTVar)
import GHC.Natural (Natural)
import Wow.Effects.TwitterStream (TwitterStream, tSSampleStream, interpretTwitterStream, interpretTwitterStreamByTChan)
import Control.Concurrent.STM (TChan, newTVarIO)
import qualified Wow.Effects.Server as S
import Wow.Effects.ClientChannel (ClientChannel, interpretClientChannel)
import Wow.Effects.Server (Server, interpretServer, ClientLookup)
import Polysemy.AtomicState (AtomicState, runAtomicStateTVar)
import qualified Data.Map.Strict as Map
import Polysemy.Input (Input, runInputSem)

filteredStreamBroadcast :: forall r . (Members [ClientChannel, STM] r, Member TwitterStream r) => TVar ServerState -> Sem r ()
filteredStreamBroadcast var = tSSampleStream broadcastC
  where
  broadcastC :: StreamEntry -> Sem r ()
  broadcastC s = do
      traceShowM s.tweet.text
      traceShowM s
      let when client = client.listening && toBool (fmap (`T.isInfixOf` s.tweet.text) client.tweetFilter)
      serverState <- atomically $ readTVar var
      broadcastSilentWhen when s.tweet.text serverState
      pure ()
    where
      toBool (Just x) = x
      toBool Nothing = True

data TwitterStreamSource
  = TSSLive
  | TSSFakeChannel (TChan StreamEntry)

data AppConfig = AppConfig {
  twitterStreamSource :: TwitterStreamSource,
  port :: Natural
}

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig {
  twitterStreamSource = TSSLive,
  port = 8131
}

type AppContraints r = (DotEnv ': STM ': Server ': ClientChannel ': TwitterStream ': WebSocket ': Finally ': HttpLongPolling ': Env ': Input ClientLookup ': AtomicState ClientLookup ':Interrupt ': Critical ':  Race ': Async ': Embed IO ': Final IO ': '[])

appToIo :: AppConfig -> Sem (AppContraints r) a -> IO a
appToIo cfg app' = do
  clientLookup <- newTVarIO Map.empty
  let
      go :: Sem (AppContraints r) a -> IO a
      go a = a
        & dotEnvToIo
        & stmToIo
        & interpretServer cfg.port
        & subsume
        & interpretClientChannel
        & subsume
        & case cfg.twitterStreamSource of
          TSSLive -> subsume . subsume . interpretTwitterStream
          TSSFakeChannel channel -> interpretTwitterStreamByTChan channel
        & webSocketToIO (go . raise_)
        & finallyToIo (go . raise_)
        & httpLongPollingToIO (go . raise_)
        & envToIo
        & runInputSem ((atomically $ readTVar clientLookup)  & stmToIo)
        & runAtomicStateTVar clientLookup
        & interpretInterruptOnce
        & interpretCritical
        & interpretRace
        & asyncToIOFinal
        & embedToFinal
        & runFinal
  go app'



main :: AppConfig -> IO ()
main cfg= do
  app cfg & appToIo cfg

app :: (  Members [ Server, ClientChannel, TwitterStream, DotEnv, STM , Finally, WebSocket, Async, Interrupt] r) => AppConfig -> Sem r ()
app _ = do
  loadDotEnv
  state <- atomically $ newTVar newServerState
  sequenceConcurrently
    [
      killOnQuit "kill stream handler" $ filteredStreamBroadcast state,
      killOnQuit "kill websocket server" $ S.start $ handleClient state
    ]
  unregister "kill stream handler"
  unregister "kill websocket server"
