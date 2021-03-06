{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# LANGUAGE NumericUnderscores #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module WowSpec (spec, withApp) where

import Wow.TestPrelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, uninterruptibleCancel, race, Async)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, newTChanIO, writeTChan, readTVarIO)
import Control.Exception (bracket, Exception, throw)
import Wow.Websocket.TestClient (startTestClient, ClientId)
import qualified Wow.WowApp as WA
import Control.Monad (forM_)
import Debug.Trace (traceM, traceShowM)
import qualified Data.Text as T
import System.Random (randomRIO)
import GHC.Natural (Natural)
import GHC.Num (integerToNatural)
import Wow.WowApp (defaultAppConfig, AppConfig, TwitterStreamSource (TSSFakeChannel))
import Wow.Twitter.Types (StreamEntry(StreamEntry, tweet, matchingRules), Tweet (Tweet, text, tweetId))

import Wow.Data.Command (Command (CmdGreeting, CmdFilter, CmdListen, CmdUnlisten, CmdClients, CmdTalk), toText)
import Wow.Data.ServerMessage (ServerMessage (SMAcknowledge, SMClientDisconnected, SMClients, SMClientJoined, SMWelcome, SMTweet, SMError, SMTalk), parseServerMessage, Error (ErrUsernameExists, ErrGreetingAlreadySucceded, ErrNotAuthenticated))
import Test.Hspec (expectationFailure)

type SendCommand = Maybe Command -> IO ()

data Timeout = Timeout Text deriving (Show, Eq)

instance Exception Timeout

type OnReceiveServerMessage = ClientId -> ServerMessage -> IO ()

startTestclientTyped :: ClientId
  -> Natural
  -> OnReceiveServerMessage
  -> IO
      (SendCommand,
      Async ())
startTestclientTyped clientId port onReceive = do
  let
    onReceive' cId msg = case parseServerMessage msg of
      Right m -> onReceive cId m
      Left l ->
        expectationFailure $ "ServerMessage couldn't be parsed: " <> show l

  (send, fiber) <- startTestClient clientId port onReceive'
  let send' = send . fmap toText
  pure (send', fiber)

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
        let config = defaultAppConfig{port, twitterStreamSource }
        a <- async $ WA.main config
        threadDelay 80_000 -- dirty, improve this by checking if the endpoint is available
        pure (a, config)
    )
    (uninterruptibleCancel . fst)
    (\(_,config) -> do
      x <- action config
      threadDelay 80_000
      pure x
    )

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
      a <- async $ if null messagesToWaitFor then do threadDelay (max 10_000 (waitTime - 50_000)) else forM_ messagesToWaitFor $ removeFromReceived messages
      action
      wait a

sendAndWaitWithWaitTime :: (Eq a, Show a) => Int -> SendCommand -> TVar [a] -> Maybe Command -> [a] -> IO ()
sendAndWaitWithWaitTime waitTime send messages msg messagesToWaitFor = do
  actionAndWaitWithWaitTime waitTime action messages messagesToWaitFor
  where
    action = send msg


sendAndWait :: (Eq a, Show a) => SendCommand -> TVar [a] -> Maybe Command -> [a] -> IO ()
sendAndWait = sendAndWaitWithWaitTime 400_000

doLogin :: SendCommand -> TVar [ServerMessage] -> IO ()
doLogin send msgs = do
  sendAndWait send msgs (Just $ CmdGreeting "Pim") [SMWelcome ["Pim"]]

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
      sendAndWait send msgs (Just $ CmdFilter "xyz") [SMAcknowledge "filter"]
      sendAndWait send msgs Nothing []
  it "should acknowledge listen command" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just CmdListen) [SMAcknowledge "listen"]
      sendAndWait send msgs Nothing []
  it "should list all clients" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just CmdClients) [SMClients ["Pim"]]
      sendAndWait send msgs Nothing []
  it "should receive listen events" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just CmdListen) [SMAcknowledge "listen"]
      let action = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "This is a tweet", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action msgs [SMTweet "This is a tweet"]
      sendAndWait send msgs Nothing []
  it "should filter events that don't match the given filter" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just CmdListen) [SMAcknowledge "listen"]
      sendAndWait send msgs (Just $ CmdFilter "abc") [SMAcknowledge "filter"]
      let action1 = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "This is a tweet", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action1 msgs []
      expectNoMessagesFor 400_000 msgs
      sendAndWait send msgs Nothing []
  it "should filter events that don't match the given filter" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just CmdListen) [SMAcknowledge "listen"]
      sendAndWait send msgs (Just $ CmdFilter "abc") [SMAcknowledge "filter"]
      let action1 = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "This is a tweet", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action1 msgs []
      (`shouldBe` []) <$> readTVarIO msgs
      let action2 = sendStreamEntryAction cfg (StreamEntry{tweet = Tweet { text = "has abc", tweetId = "1" }, matchingRules = Nothing})
      actionAndWait action2 msgs [SMTweet "has abc"]
      (`shouldBe` []) <$> readTVarIO msgs
      sendAndWait send msgs Nothing []
  it "should acknowledge unlisten command" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      doLogin send msgs
      sendAndWait send msgs (Just CmdListen) [SMAcknowledge "listen"]
      sendAndWait send msgs (Just CmdUnlisten) [SMAcknowledge "unlisten"]
      sendAndWait send msgs Nothing []
  it "should allow Clients to login" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      sendAndWait send msgs (Just $ CmdGreeting "Pim") [SMWelcome ["Pim"]]
      sendAndWait send msgs Nothing []
  it "should send error when trying to run a command without greeting" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      sendAndWait send msgs (Just CmdListen) [SMError ErrNotAuthenticated]
      sendAndWait send msgs Nothing []
  it "should send unexpected command when trying to greet twice" . withApp $ \cfg -> do
    withClient cfg.port "Pim" $ \(send, msgs) -> do
      sendAndWait send msgs (Just $ CmdGreeting "Pim") [SMWelcome ["Pim"]]
      sendAndWait send msgs (Just $ CmdGreeting "Pim") [SMError ErrGreetingAlreadySucceded]
      sendAndWait send msgs Nothing []
  it "should allow multiple Clients to login" . withApp $ \cfg -> do
    withClients2 cfg.port ("Pim", "Wim") $ \(send1, send2, msgs) -> do
      sendAndWait send1 msgs (Just $ CmdGreeting "Pim") [("Pim", SMWelcome ["Pim"])]
      sendAndWait send2 msgs (Just $ CmdGreeting "Wim") [("Wim", SMWelcome ["Wim", "Pim"]), ("Pim",SMClientJoined "Wim")]
      sendAndWait send1 msgs Nothing [("Wim",SMClientDisconnected "Pim")]
      sendAndWait send2 msgs Nothing []
  it "should allow multiple Clients to talk to each other" . withApp $ \cfg -> do
    withClients2 cfg.port ("Pim", "Wim") $ \(send1, send2, msgs) -> do
      sendAndWait send1 msgs (Just $ CmdGreeting "Pim") [("Pim", SMWelcome ["Pim"])]
      sendAndWait send2 msgs (Just $ CmdGreeting "Wim") [("Wim", SMWelcome ["Wim", "Pim"]), ("Pim", SMClientJoined "Wim")]
      sendAndWait send1 msgs (Just $ CmdTalk "Hi Wim") [("Wim", SMTalk "Pim" "Hi Wim")]
      sendAndWait send2 msgs (Just $ CmdTalk "Hello there") [("Pim", SMTalk "Wim" "Hello there")]
      sendAndWait send1 msgs Nothing [("Wim",SMClientDisconnected "Pim")]
      sendAndWait send2 msgs Nothing []
  it "should not allow the same name twice" . withApp $ \cfg -> do
    withClients2 cfg.port ("Pim1", "Pim2") $ \(send1, send2, msgs) -> do
      sendAndWait send1 msgs (Just $ CmdGreeting "Pim") [("Pim1", SMWelcome ["Pim"])]
      sendAndWait send2 msgs (Just $ CmdGreeting "Pim") [("Pim2", SMError ErrUsernameExists)]
      sendAndWait send1 msgs Nothing []
      sendAndWait send2 msgs Nothing []

withClient :: Natural -> ClientId -> ((SendCommand, TVar [ServerMessage]) -> IO ()) -> IO ()
withClient port c1 action = do
  msgs <- newTVarIO []
  let
    onReceive _ msg = atomically $ modifyTVar msgs (msg :)
  (send1, fiber1) <- startTestclientTyped c1 port onReceive

  action (send1, msgs)
  wait fiber1

withClients2 :: Natural -> (ClientId, ClientId) -> ((SendCommand, SendCommand, TVar [(ClientId, ServerMessage)]) -> IO ()) -> IO ()
withClients2 port (c1, c2) action = do
  msgs <- newTVarIO []
  let
    onReceive clientId msg = atomically $ modifyTVar msgs ((clientId, msg) :)
  (send1, fiber1) <- startTestclientTyped c1 port onReceive
  (send2, fiber2) <- startTestclientTyped c2 port onReceive
  action (send1, send2, msgs)
  wait fiber1
  wait fiber2
