{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use readTVarIO" #-}
module Wow.Websocket where

import Prelude
import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc (readTVar, TVar)
import UnliftIO (modifyTVar, MonadUnliftIO, toIO)
import Control.Monad (forM_)
import Control.Monad.Cont (forever)
import Debug.Trace (traceShowM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Polysemy (Sem, Member)
import Wow.Effects.WebSocket (WebSocket, withPingThread, receiveData, sendTextData, acceptRequest)
import Wow.Effects.Finally (finally, Finally)
import Wow.Effects.STM (STM, atomically)
import Wow.Data.Command (parseCommand, Command (CmdGreeting, CmdClients, CmdListen, CmdFilter, CmdTalk, CmdUnlisten))

data Client = Client {
  name :: Text,
  conn:: WS.Connection,
  listening :: Bool,
  tweetFilter :: Maybe Text
  }

instance Show Client where
  show c = "Client { name = " <> show c.name <> ", listening = " <> show c.listening <> ", tweetFilter = " <> show c.tweetFilter <> "  }"

data ServerState = ServerState {
  clients :: [Client]
} deriving (Show)

newServerState :: ServerState
newServerState = ServerState {
  clients = []
}
numClients :: ServerState -> Int
numClients = length . (.clients)

clientExists :: Client -> ServerState -> Bool
clientExists c = any ((== c.name) . (.name)) . (.clients)

nameExists :: Text -> ServerState -> Bool
nameExists name = any ((== name) . (.name)) . (.clients)

addClient :: Client -> ServerState -> ServerState
addClient c s = s{clients = c:s.clients }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = s{ clients = filter ((/= client.name) . (.name)) s.clients }

updateClient :: (Client -> Client) -> ServerState -> ServerState
updateClient update s = s{clients = fmap update s.clients}

updateClientByName :: Client -> (Client -> Client) -> ServerState -> ServerState
updateClientByName c update = updateClient (set c.name)
  where
    set name client
      | client.name /= name = client
      | otherwise = update client

setClientListening :: Client -> Bool -> ServerState -> ServerState
setClientListening c listening = updateClientByName c $ \cl -> cl{listening}

setClientFilter :: Client -> Maybe Text -> ServerState -> ServerState
setClientFilter c tweetFilter = updateClientByName c $ \cl -> cl{tweetFilter}

broadcastSilent :: (Member WebSocket r) => Text -> ServerState -> Sem r ()
broadcastSilent message s = do
  forM_ s.clients $ \c -> sendTextData c.conn message

broadcastSilentWhen :: (Member WebSocket r) => (Client -> Bool) -> Text -> ServerState -> Sem r ()
broadcastSilentWhen f message s = do
  forM_ s.clients sendIf
  where
    sendIf c = if f c then sendTextData c.conn message else pure ()

broadcast :: (Member WebSocket r) => Text -> ServerState -> Sem r ()
broadcast message s = do
  traceShowM message
  broadcastSilent message s

withPingThreadUnliftIO :: (MonadIO m, MonadUnliftIO m) => WS.Connection -> Int -> m () -> m a -> m a
withPingThreadUnliftIO conn interval pingAction appAction = do
  pingAction' <- toIO pingAction
  appAction' <- toIO appAction
  liftIO $ WS.withPingThread conn interval pingAction' appAction'

webSocketApp :: forall r. (Member STM r, Member Finally r, Member WebSocket r) => TVar ServerState -> WS.PendingConnection -> Sem r ()
webSocketApp state pending = do
  traceShowM ("incoming connection"::Text)
  traceShowM (WS.pendingRequest pending)
  conn <- acceptRequest pending
  traceShowM ("after accept"::Text)

  withPingThread conn 30 (pure ()) $ do
    msg <- receiveData conn
    case parseCommand msg of
      Left e -> do
          traceShowM e
          sendTextData conn ("Wrong announcement" :: Text)
      Right (CmdGreeting n) -> flip finally disconnect $ do
        res <- atomically $ do
          s' <- readTVar state
          if nameExists n s' then
            pure Nothing
          else do
            modifyTVar state $ addClient client
            s <- readTVar state
            pure $ Just (s', s)
        case res of
          Nothing -> sendTextData conn ("User already exists" :: Text)
          Just (s', s) -> do
            sendTextData conn $
              "Welcome! Users: " <>
              T.intercalate ", " (map (.name) s.clients)
            broadcast (client.name <> " joined") s'
            talk client state
        where
          client = Client { name = n, conn, listening = False, tweetFilter = Nothing }
          disconnect = do
            traceShowM ("disconnect"::Text)
            s <- atomically $ do
              modifyTVar state $ \s -> removeClient client s
              readTVar state
            traceShowM ("disconnect broadcast..."::Text)
            traceShowM s
            broadcast (client.name <> " disconnected") s
      Right _ ->
        sendTextData conn ("Unexpected command" :: Text)

talk :: (Member STM r, Member WebSocket r) => Client -> TVar ServerState -> Sem r ()
talk c state = forever $ do
  msg <- receiveData c.conn
  case parseCommand msg of
    Right CmdClients -> do
          s <- atomically $ readTVar state
          sendTextData c.conn $ "All users: " <> T.intercalate ", " (map (.name) s.clients)
          pure ()
    Right CmdListen -> do
          sendTextData c.conn ("listen acknowledged."::Text)
          atomically $ modifyTVar state $ setClientListening c True
          pure ()
    Right (CmdFilter f) -> do
          sendTextData c.conn ("filter acknowledged."::Text)
          atomically $ modifyTVar state $ setClientFilter c (Just f)
          pure ()
    Right CmdUnlisten -> do
          sendTextData c.conn ("unlisten acknowledged."::Text)
          atomically $ modifyTVar state $ setClientListening c False
          pure ()
    Right (CmdTalk msg) ->
          (atomically $ readTVar state) >>= broadcast (c.name `mappend` ": " `mappend` msg)
    Right (CmdGreeting _) ->
          sendTextData c.conn ("Greeting already succeeded" :: Text)
    Left _ ->
          sendTextData c.conn ("Unexpected command" :: Text)