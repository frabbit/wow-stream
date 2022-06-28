{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# HLINT ignore "Use newTVarIO" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Wow.WowApp where

import Control.Concurrent.STM (TChan, newTVarIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Debug.Trace (traceShowM)
import GHC.Conc (readTVar)
import GHC.Natural (Natural)
import Polysemy (Embed, Final, Members, Sem, embedToFinal, raise_, runFinal, subsume)
import Polysemy.Async (Async, asyncToIOFinal, sequenceConcurrently)
import Polysemy.AtomicState (AtomicState, runAtomicStateTVar)
import Polysemy.Conc (Critical, Race, interpretCritical, interpretInterruptOnce, interpretRace)
import Polysemy.Conc.Effect.Interrupt (Interrupt, killOnQuit, unregister)
import Polysemy.Input (Input, runInputSem, input)
import Wow.Data.ServerMessage (ServerMessage (SMTweet))
import Wow.Data.ServerState (ServerState, newServerState)
import Wow.Effects.ClientChannel (ClientChannel, interpretClientChannel)
import Wow.Effects.DotEnv (DotEnv, dotEnvToIo, loadDotEnv)
import Wow.Effects.Env (Env, envToIo)
import Wow.Effects.Finally (Finally, finallyToIo)
import Wow.Effects.HttpLongPolling (HttpLongPolling, httpLongPollingToIO)
import Wow.Effects.STM (STM, atomically, stmToIo)
import Wow.Effects.Server (ClientLookup, Server, interpretServer)
import qualified Wow.Effects.Server as S
import Wow.Effects.TwitterStream (TwitterStream, interpretTwitterStream, interpretTwitterStreamByTChan, tSSampleStream)
import Wow.Effects.WebSocket (WebSocket, webSocketToIO)
import Wow.Prelude
import Wow.Twitter.Types (StreamEntry)
import Wow.Broadcasting (broadcastSilentWhen)
import Wow.Effects.ServerApi (ServerApi, interpretServerApi)
import Wow.Websocket (handleClient)

filteredStreamBroadcast :: forall r. (Members '[ClientChannel, STM, Input ServerState, AtomicState ServerState, TwitterStream] r) => Sem r ()
filteredStreamBroadcast = tSSampleStream broadcastC
  where
    broadcastC :: StreamEntry -> Sem r ()
    broadcastC s = do
      traceShowM s.tweet.text
      traceShowM s
      let when client = client.listening && toBool (fmap (`T.isInfixOf` s.tweet.text) client.tweetFilter)
      serverState <- input @ServerState
      broadcastSilentWhen when (SMTweet s.tweet.text) serverState
      pure ()
      where
        toBool (Just x) = x
        toBool Nothing = True

data TwitterStreamSource
  = TSSLive
  | TSSFakeChannel (TChan StreamEntry)

data AppConfig = AppConfig
  { twitterStreamSource :: TwitterStreamSource,
    port :: Natural
  }

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { twitterStreamSource = TSSLive,
      port = 8131
    }

type AppModules r =
  '[ ServerApi,
     DotEnv,
     STM,
     Server,
     ClientChannel,
     TwitterStream,
     WebSocket,
     Finally,
     HttpLongPolling,
     Env,
     Input ClientLookup,
     Input ServerState,
     AtomicState ClientLookup,
     AtomicState ServerState,
     Interrupt,
     Critical,
     Race,
     Async,
     Embed IO,
     Final IO
   ]

appToIo :: AppConfig -> Sem (AppModules r) a -> IO a
appToIo cfg app' = do
  clientLookup <- newTVarIO Map.empty
  serverState <- newTVarIO newServerState
  let go :: Sem (AppModules r) a -> IO a
      go a =
        a
          & interpretServerApi
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
          & runInputSem ((atomically $ readTVar clientLookup) & stmToIo)
          & runInputSem ((atomically $ readTVar serverState) & stmToIo)
          & runAtomicStateTVar clientLookup
          & runAtomicStateTVar serverState
          & interpretInterruptOnce
          & interpretCritical
          & interpretRace
          & asyncToIOFinal
          & embedToFinal
          & runFinal
  go app'

main :: AppConfig -> IO ()
main cfg = do
  app cfg & appToIo cfg

app :: (Members [ServerApi, Server, Input ServerState, AtomicState ServerState, ClientChannel, TwitterStream, DotEnv, STM, Finally, WebSocket, Async, Interrupt] r) => AppConfig -> Sem r ()
app _ = do
  loadDotEnv
  sequenceConcurrently
    [ killOnQuit "kill stream handler" filteredStreamBroadcast,
      killOnQuit "kill websocket server" $ S.start handleClient
    ]
  unregister "kill stream handler"
  unregister "kill websocket server"
