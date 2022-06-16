{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# LANGUAGE NumericUnderscores #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module WowSpec (spec, withApp) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, uninterruptibleCancel, race)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, putTMVar, readTVar, TMVar, newTChan, newTChanIO, writeTChan, readTVarIO)
import Control.Exception (bracket, Exception, throw)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Wow.Websocket.TestClient (startTestClient, ClientId)
import qualified Wow.WowApp as WA
import Prelude
import Control.Monad (forM_)
import Debug.Trace (traceM, traceShowM)
import qualified Data.Text as T
import System.Random (randomRIO)
import GHC.Natural (Natural)
import GHC.Num (integerToNatural)
import Wow.WowApp (defaultAppConfig, AppConfig, TwitterStreamSource (TSSFakeChannel))
import Wow.Twitter.Types (StreamEntry(StreamEntry, tweet, matchingRules), Tweet (Tweet, text, tweetId))

data Timeout = Timeout Text deriving (Show, Eq)

instance Exception Timeout

removeFromReceived :: Eq a => TVar [a] -> a -> IO ()
removeFromReceived allReceived msg = do
  removed <- atomically $ do
    b <- length <$> readTVar allReceived
    modifyTVar allReceived (filter (/= msg))
    a <- length <$> readTVar allReceived
    pure (a == b - 1)
  if removed
    then pure ()
    else do
      threadDelay 10
      removeFromReceived allReceived msg

withApp :: (AppConfig -> IO c) -> IO c
withApp action = do
  bracket
    ( do
        (r::Integer) <- randomRIO (1_024, 49_151)
        let port = integerToNatural r
        twitterStreamSource <- WA.TSSFakeChannel <$> newTChanIO
        traceShowM port
        let config = defaultAppConfig{port, twitterStreamSource }
        a <- async $ WA.main config
        threadDelay 50_000 -- dirty, improve this by checking if the endpoint is available
        pure (a, config)
    )
    (uninterruptibleCancel . fst)
    (\(_,config) -> do action config)

expectNoMessagesFor :: (Eq a, Show a) => Int -> TVar [a] -> IO ()
expectNoMessagesFor waitTime messages = go waitTime
  where
    go t = do
      x <- atomically $ readTVar messages
      x `shouldBe` []
      if t == 0 then
        pure ()
      else do
        let w = (if waitTime < 10_000 then waitTime else 10_000)
        threadDelay w
        go (t - w)


actionAndWait :: (Eq a, Show a) => IO () -> TVar [a] -> [a] -> IO ()
actionAndWait = actionAndWaitWithWaitTime 400_000

actionAndWaitWithWaitTime :: (Eq a, Show a) => Int -> IO () -> TVar [a] -> [a] -> IO ()
actionAndWaitWithWaitTime waitTime action messages messagesToWaitFor = do
  winner <- race f $ do
    threadDelay waitTime
    pure ()
  case winner of
    Right () -> do
      traceM "timeout"
      msgs <- atomically $ readTVar messages
      throw . Timeout . T.pack $ "i was waiting for " <> show messagesToWaitFor <> " but only received " <> show msgs
    Left _ -> pure ()
  pure ()
  where
    f = do
      a <- async $ forM_ messagesToWaitFor $ removeFromReceived messages
      action
      wait a

sendAndWaitWithWaitTime :: (Eq a, Show a) => Int -> TMVar (Maybe Text) -> TVar [a] -> Maybe Text -> [a] -> IO ()
sendAndWaitWithWaitTime waitTime send messages msg messagesToWaitFor = do
  actionAndWaitWithWaitTime waitTime action messages messagesToWaitFor
  where
    action = atomically $ putTMVar send msg


sendAndWait :: (Eq a, Show a) => TMVar (Maybe Text) -> TVar [a] -> Maybe Text -> [a] -> IO ()
sendAndWait = sendAndWaitWithWaitTime 400_000

doLogin :: TMVar (Maybe Text) -> TVar [Text] -> IO ()
doLogin send msgs = do
  sendAndWait send msgs (Just ":greeting Pim") ["Welcome! Users: Pim"]

sendStreamEntryAction :: AppConfig -> StreamEntry -> IO ()
sendStreamEntryAction cfg entry =
  case cfg.twitterStreamSource of
    TSSFakeChannel channel -> do
      atomically $ writeTChan channel entry
    _ -> error "unexpected twitterStreamSource"

spec :: Spec
spec = describe "WowApp" $ do
  it "should acknowledge filter command" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just ":filter xyz") ["filter acknowledged."]
      sendAndWait send msgs Nothing []
  it "should acknowledge listen command" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just ":listen") ["listen acknowledged."]
      sendAndWait send msgs Nothing []
  it "should receive listen events" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just ":listen") ["listen acknowledged."]
      let action = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "This is a tweet", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action msgs ["This is a tweet"]
      sendAndWait send msgs Nothing []
  it "should filter events that don't match the given filter" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just ":listen") ["listen acknowledged."]
      sendAndWait send msgs (Just ":filter abc") ["filter acknowledged."]
      let action1 = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "This is a tweet", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action1 msgs []
      expectNoMessagesFor 400_000 msgs
      sendAndWait send msgs Nothing []
  it "should filter events that don't match the given filter" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just ":listen") ["listen acknowledged."]
      sendAndWait send msgs (Just ":filter abc") ["filter acknowledged."]
      let action1 = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "This is a tweet", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action1 msgs []
      (`shouldBe` []) <$> readTVarIO msgs
      let action2 = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "has abc", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action2 msgs ["has abc"]
      (`shouldBe` []) <$> readTVarIO msgs
      sendAndWait send msgs Nothing []
  it "should acknowledge unlisten command" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just ":listen") ["listen acknowledged."]
      sendAndWait send msgs (Just ":unlisten") ["unlisten acknowledged."]
      sendAndWait send msgs Nothing []
  it "should allow Clients to login" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      sendAndWait send msgs (Just ":greeting Pim") ["Welcome! Users: Pim"]
      sendAndWait send msgs Nothing []
  it "should allow multiple Clients to login" . withApp $ \cfg -> do
    withClients2 cfg.port ("Pim", "Wim") $ \(send1, send2, msgs) -> do
      sendAndWait send1 msgs (Just ":greeting Pim") [("Pim", "Welcome! Users: Pim")]
      sendAndWait send2 msgs (Just ":greeting Wim") [("Wim", "Welcome! Users: Wim, Pim"), ("Pim","Wim joined")]
      sendAndWait send1 msgs Nothing [("Wim","Pim disconnected")]
      sendAndWait send2 msgs Nothing []

withClient :: Natural -> ClientId -> ((TMVar (Maybe Text), TVar [Text]) -> IO ()) -> IO ()
withClient port c1 action = do
  msgs <- newTVarIO []
  let
    onReceive _ msg = atomically $ modifyTVar msgs (msg :)
  (send1, fiber1) <- startTestClient c1 port onReceive

  action (send1, msgs)
  wait fiber1

withClients2 :: Natural -> (ClientId, ClientId) -> ((TMVar (Maybe Text), TMVar (Maybe Text), TVar [(ClientId, Text)]) -> IO ()) -> IO ()
withClients2 port (c1, c2) action = do
  msgs <- newTVarIO []
  let
    onReceive clientId msg = atomically $ modifyTVar msgs ((clientId, msg) :)
  (send1, fiber1) <- startTestClient c1 port onReceive
  (send2, fiber2) <- startTestClient c2 port onReceive
  action (send1, send2, msgs)
  wait fiber1
  wait fiber2