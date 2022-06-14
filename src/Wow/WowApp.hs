{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# HLINT ignore "Use newTVarIO" #-}
module Wow.WowApp where

import qualified Data.Text as T
import Prelude
import qualified Wow.Effects.WebSocket as WSE
import Wow.Websocket (newServerState, broadcastSilentWhen, ServerState, webSocketApp)
import GHC.Conc (TVar, readTVar)
import Wow.Twitter.Types (StreamEntry)

import Polysemy (Sem, Member, Embed, embedToFinal, runFinal, Final, raise_)
import Wow.Effects.HttpLongPolling (HttpLongPolling, httpLongPollingToIO)
import Wow.Effects.Env (Env, envToIo)
import Wow.Twitter.FilteredStream (filteredStream)
import Polysemy.Async (Async, asyncToIOFinal, sequenceConcurrently)
import Data.Function ((&))
import Polysemy.Conc.Effect.Interrupt (Interrupt, killOnQuit, unregister)
import Polysemy.Conc (Critical, Race, interpretCritical, interpretRace, interpretInterruptOnce)
import Wow.Effects.WebSocket (WebSocket, webSocketToIO)
import Wow.Effects.Finally (Finally, finallyToIo)
import Wow.Effects.DotEnv (DotEnv, loadDotEnv, dotEnvToIo)
import Wow.Effects.STM (STM, stmToIo, atomically)
import GHC.Conc.Sync (newTVar)

filteredStreamBroadcast :: forall r . (Member Env r, Member STM r, Member HttpLongPolling r, Member WebSocket r) => TVar ServerState -> Sem r ()
filteredStreamBroadcast var = filteredStream broadcastC
  where
  broadcastC :: _ => StreamEntry -> Sem r ()
  broadcastC s = do
      let when client = client.listening && toBool (fmap (`T.isInfixOf` s.tweet.text) client.tweetFilter)
      serverState <- atomically $ readTVar var
      broadcastSilentWhen when s.tweet.text serverState
      pure ()
    where
      toBool (Just x) = x
      toBool Nothing = True

type AppContraints r = (DotEnv ': STM ': WebSocket ': Finally ': HttpLongPolling ': Env ': Interrupt ': Critical ':  Race ': Async ': Embed IO ': Final IO ': '[])
appToIo :: Sem (AppContraints r) a -> IO a
appToIo app' = app'
    & dotEnvToIo
    & stmToIo
    & webSocketToIO (appToIo . raise_)
    & finallyToIo (appToIo . raise_)
    & httpLongPollingToIO (appToIo . raise_)
    & envToIo
    & interpretInterruptOnce
    & interpretCritical
    & interpretRace
    & asyncToIOFinal
    & embedToFinal
    & runFinal


main :: IO ()
main = do
  app & appToIo

app :: (  Member DotEnv r, Member Env r, Member STM r, Member Finally r, Member WebSocket r, Member Async r, Member HttpLongPolling r, Member Interrupt r) =>Sem r ()
app = do
  loadDotEnv
  state <- atomically $ newTVar newServerState
  sequenceConcurrently
    [
      killOnQuit "kill stream handler" $ filteredStreamBroadcast state,
      killOnQuit "kill websocket server" $ WSE.runServer "127.0.0.1" 8131 $ webSocketApp state
    ]
  unregister "kill stream handler"
  unregister "kill websocket server"


