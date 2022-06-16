{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# HLINT ignore "Use newTVarIO" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Wow.WowApp where


import Debug.Trace (traceShowM)
import qualified Data.Text as T
import Prelude
import qualified Wow.Effects.WebSocket as WSE
import Wow.Websocket (newServerState, broadcastSilentWhen, ServerState, webSocketApp)
import GHC.Conc (TVar, readTVar)
import Wow.Twitter.Types (StreamEntry)

import Polysemy (Sem, Member, Embed, embedToFinal, runFinal, Final, raise_, subsume)
import Wow.Effects.HttpLongPolling (HttpLongPolling, httpLongPollingToIO)
import Wow.Effects.Env (Env, envToIo)
import Polysemy.Async (Async, asyncToIOFinal, sequenceConcurrently)
import Data.Function ((&))
import Polysemy.Conc.Effect.Interrupt (Interrupt, killOnQuit, unregister)
import Polysemy.Conc (Critical, Race, interpretCritical, interpretRace, interpretInterruptOnce)
import Wow.Effects.WebSocket (WebSocket, webSocketToIO)
import Wow.Effects.Finally (Finally, finallyToIo)
import Wow.Effects.DotEnv (DotEnv, loadDotEnv, dotEnvToIo)
import Wow.Effects.STM (STM, stmToIo, atomically)
import GHC.Conc.Sync (newTVar)
import GHC.Natural (Natural, naturalToInteger)
import Wow.Effects.TwitterStream (TwitterStream, tSSampleStream, interpretTwitterStream, interpretTwitterStreamByTChan)
import Control.Concurrent.STM (TChan)

filteredStreamBroadcast :: forall r . (Member STM r, Member WebSocket r, Member TwitterStream r) => TVar ServerState -> Sem r ()
filteredStreamBroadcast var = tSSampleStream broadcastC
  where
  broadcastC :: _ => StreamEntry -> Sem r ()
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

type AppContraints r = (DotEnv ': STM ': TwitterStream ': WebSocket ': Finally ': HttpLongPolling ': Env ': Interrupt ': Critical ':  Race ': Async ': Embed IO ': Final IO ': '[])

appToIo :: AppConfig -> Sem (AppContraints r) a -> IO a
appToIo cfg app' = app'
    & dotEnvToIo
    & stmToIo
    & case cfg.twitterStreamSource of
      TSSLive -> subsume . subsume . interpretTwitterStream
      TSSFakeChannel channel -> interpretTwitterStreamByTChan channel
    & webSocketToIO (appToIo cfg . raise_)
    & finallyToIo (appToIo cfg . raise_)
    & httpLongPollingToIO (appToIo cfg . raise_)
    & envToIo
    & interpretInterruptOnce
    & interpretCritical
    & interpretRace
    & asyncToIOFinal
    & embedToFinal
    & runFinal


main :: AppConfig -> IO ()
main cfg= do
  app cfg & appToIo cfg

app :: (  Member TwitterStream r, Member DotEnv r, Member STM r, Member Finally r, Member WebSocket r, Member Async r, Member Interrupt r) => AppConfig -> Sem r ()
app cfg = do
  loadDotEnv
  state <- atomically $ newTVar newServerState
  sequenceConcurrently
    [
      killOnQuit "kill stream handler" $ filteredStreamBroadcast state,
      killOnQuit "kill websocket server" $ WSE.runServer "127.0.0.1" (fromInteger $ naturalToInteger cfg.port) $ webSocketApp state
    ]
  unregister "kill stream handler"
  unregister "kill websocket server"


