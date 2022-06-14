{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Wow.WowApp where

import qualified Data.Text as T
import Prelude
import qualified Wow.Effects.WebSocket as WSE
import Wow.Websocket (newServerState, broadcastSilentWhen, ServerState, webSocketApp)
import GHC.Conc (newTVarIO, readTVarIO, TVar)
import Wow.Twitter.Types (StreamEntry, loadDotEnv)

import Conduit (MonadIO (..))
import Polysemy (Sem, Member, Embed, embedToFinal, runFinal, Final, raise, raise_)
import Wow.Effects.HttpLongPolling (HttpLongPolling, httpLongPollingToIO)
import Wow.Twitter.FilteredStream (filteredStream)
import Polysemy.Async (Async, async, asyncToIOFinal, await)
import Data.Function ((&))
import Polysemy.Conc.Effect.Interrupt (Interrupt, killOnQuit)
import Polysemy.Conc (interpretInterrupt, Critical, Race, interpretCritical, interpretRace)
import Wow.Effects.WebSocket (WebSocket, webSocketToIO)
import Wow.Effects.Finally (Finally, finallyToIo)
import Wow.Effects.STM (STM, stmToIo)

filteredStreamBroadcast :: forall r . (Member HttpLongPolling r, Member (Embed IO) r) => TVar ServerState -> Sem r ()
filteredStreamBroadcast var = filteredStream broadcastC
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

type AppContraints r = (STM ': WebSocket ': Finally ': HttpLongPolling ':Interrupt ': Critical ':  Race ': Async ': Embed IO ': Final IO ': '[])
appToIo :: Sem (AppContraints r) a -> IO a
appToIo app' = app'
    & stmToIo
    & webSocketToIO (appToIo . raise_)
    & finallyToIo (appToIo . raise_)
    & httpLongPollingToIO (appToIo . raise_)
    & interpretInterrupt
    & interpretCritical
    & interpretRace
    & asyncToIOFinal
    & embedToFinal
    & runFinal


main :: IO ()
main = do
  app & appToIo

app :: (  Member STM r, Member Finally r, Member WebSocket r, Member Async r, Member (Embed IO) r, Member HttpLongPolling r, Member Interrupt r) =>Sem r ()
app = do
  liftIO loadDotEnv
  state <- liftIO $ newTVarIO newServerState
  a1 <- async $ killOnQuit "arg" $ filteredStreamBroadcast state
  a2 <- async $ killOnQuit "arg" $ WSE.runServer "127.0.0.1" 8131 $ webSocketApp state
  await a1
  await a2
  pure ()


