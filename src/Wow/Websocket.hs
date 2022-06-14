{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use readTVarIO" #-}
module Wow.Websocket where

import Prelude
import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Conc (readTVar, TVar, readTVarIO)
import Data.Char (isSpace, isPunctuation)
import UnliftIO (modifyTVar, MonadUnliftIO, toIO)
import Control.Monad (forM_)
import Control.Monad.Cont (forever)
import Debug.Trace (traceShowM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Polysemy (Sem, Member, Embed)
import Wow.Effects.WebSocket (WebSocket, withPingThread, receiveData, sendTextData, acceptRequest)
import Wow.Effects.Finally (finally, Finally)
import Wow.Effects.STM (STM, atomically)

data Client = Client {
  name :: Text,
  conn:: WS.Connection,
  listening :: Bool,
  tweetFilter :: Maybe Text
  }

instance Show Client where
  show c = "Client { name = " <> show c.name <> ", listening = " <> show c.listening <> ", tweetFilter = " <> show c.tweetFilter <> " }"

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

addClient :: Client -> ServerState -> ServerState
addClient c s = s{clients = c:s.clients }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = s{ clients = filter ((/= client.name) . (.name)) s.clients }

updateClient :: (Client -> Client) -> ServerState -> ServerState
updateClient update s = s{clients = fmap update s.clients}

setClientListening :: Client -> Bool -> ServerState -> ServerState
setClientListening c listening = updateClient (set c.name)
  where
    set name client
      | client.name /= name = client
      | otherwise = client{listening = listening}

setClientFilter :: Client -> Maybe Text -> ServerState -> ServerState
setClientFilter c tweetFilter = updateClient (set c.name)
  where
    set name client
      | client.name /= name = client
      | otherwise = client{tweetFilter = tweetFilter}

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
  traceShowM "incoming connection"
  traceShowM (WS.pendingRequest pending)
  conn <- acceptRequest pending
  traceShowM ("after accept")

  withPingThread conn 30 (pure ()) $ do
    msg <- receiveData conn
    clients <- atomically $ readTVar state
    case msg of
      _ | not (prefix `T.isPrefixOf` msg) ->
          sendTextData conn ("Wrong announcement" :: Text)
        | any ($ client.name)
          [T.null, T.any isPunctuation, T.any isSpace] ->
              sendTextData conn ("Name cannot " <>
                "contain punctuation or whitespace, and " <>
                "cannot be empty" :: Text)
        | clientExists client clients -> sendTextData conn ("User already exists" :: Text)
        | otherwise -> flip finally disconnect $ do
            (s', s) <- atomically $ do
              s' <- readTVar state
              modifyTVar state $ addClient client
              s <- readTVar state
              pure (s', s)
            sendTextData conn $
                "Welcome! Users: " <>
                T.intercalate ", " (map (.name) s.clients)
            broadcast (client.name <> " joined") s'
            talk client state
        where
          prefix =":greeting "
          client = Client { name = T.drop (T.length prefix) msg, conn, listening = False, tweetFilter = Nothing }
          disconnect = do
            traceShowM "disconnect"
            s <- atomically $ do
              modifyTVar state $ \s -> removeClient client s
              readTVar state
            traceShowM "disconnect broadcast..."
            traceShowM s
            broadcast (client.name <> " disconnected") s

talk :: (Member STM r, Member WebSocket r) => Client -> TVar ServerState -> Sem r ()
talk c state = forever $ do
  msg <- receiveData c.conn
  case msg of
    _ | msg == ":clients" -> do
          s <- atomically $ readTVar state
          sendTextData c.conn $ "All users: " <> T.intercalate ", " (map (.name) s.clients)
          pure ()
    _ | msg == ":listen" -> do
          sendTextData c.conn ("listen acknowledged."::Text)
          atomically $ modifyTVar state $ setClientListening c True
          pure ()
    _ | ":filter " `T.isPrefixOf` msg -> do
          let filterWord = T.drop 8 msg
          sendTextData c.conn ("filter acknowledged."::Text)
          atomically $ modifyTVar state $ setClientFilter c (Just filterWord)
          pure ()
    _ | msg == ":unlisten" -> do
          sendTextData c.conn ("unlisten acknowledged."::Text)
          atomically $ modifyTVar state $ setClientListening c False
          pure ()
      | otherwise -> (atomically $ readTVar state) >>= broadcast (c.name `mappend` ": " `mappend` msg)