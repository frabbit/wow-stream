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
import GHC.Conc (TVar, readTVar)
import GHC.Conc.Sync (newTVar)
import GHC.Natural (Natural)
import Polysemy (Embed, Final, Member, Members, Sem, embedToFinal, raise_, runFinal, subsume)
import Polysemy.Async (Async, asyncToIOFinal, sequenceConcurrently)
import Polysemy.AtomicState (AtomicState, runAtomicStateTVar)
import Polysemy.Conc (Critical, Race, interpretCritical, interpretInterruptOnce, interpretRace)
import Polysemy.Conc.Effect.Interrupt (Interrupt, killOnQuit, unregister)
import Polysemy.Input (Input, runInputSem)
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
import Wow.Websocket (broadcastSilentWhen, handleClient)

filteredStreamBroadcast :: forall r. (Members [ClientChannel, STM] r, Member TwitterStream r) => TVar ServerState -> Sem r ()
filteredStreamBroadcast var = tSSampleStream broadcastC
  where
    broadcastC :: StreamEntry -> Sem r ()
    broadcastC s = do
      traceShowM s.tweet.text
      traceShowM s
      let when client = client.listening && toBool (fmap (`T.isInfixOf` s.tweet.text) client.tweetFilter)
      serverState <- atomically $ readTVar var
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
  '[ DotEnv,
     STM,
     Server,
     ClientChannel,
     TwitterStream,
     WebSocket,
     Finally,
     HttpLongPolling,
     Env,
     Input ClientLookup,
     AtomicState ClientLookup,
     AtomicState ServerState,
     Interrupt,
     Critical,
     Race,
     Async,
     Embed IO,
     Final IO
   ]

appToIo :: AppConfig -> _ -> Sem (AppModules r) a -> IO a
appToIo cfg serverState app' = do
  clientLookup <- newTVarIO Map.empty
  let go :: Sem (AppModules r) a -> IO a
      go a =
        a
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
  state <- newTVarIO newServerState
  app cfg state & appToIo cfg state

app :: (Members [Server, ClientChannel, TwitterStream, DotEnv, STM, Finally, WebSocket, Async, Interrupt] r) => AppConfig -> _ -> Sem r ()
app _ state = do
  loadDotEnv
  sequenceConcurrently
    [ killOnQuit "kill stream handler" $ filteredStreamBroadcast state,
      killOnQuit "kill websocket server" $ S.start $ handleClient state
    ]
  unregister "kill stream handler"
  unregister "kill websocket server"
