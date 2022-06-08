{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Wow.Websocket where

import Prelude
import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Conc (atomically, readTVar, TVar, newTVarIO, readTVarIO)
import Data.Char (isSpace, isPunctuation)
import UnliftIO (modifyTVar)
import Control.Exception (finally)
import Control.Monad (forM_)
import Control.Monad.Cont (forever)

data Client = Client {
  name :: Text,
  conn:: WS.Connection
  }

data ServerState = ServerState {
  clients :: [Client]
}

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


broadcast :: Text -> ServerState -> IO ()
broadcast message s = do
  T.putStrLn message
  forM_ s.clients $ \c -> WS.sendTextData c.conn message

main :: IO ()
main = do
  state <- newTVarIO newServerState
  WS.runServer "127.0.0.1" 8130 $ application state

application :: TVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
      msg <- WS.receiveData conn
      clients <- readTVarIO state
      case msg of
        _ | not (prefix `T.isPrefixOf` msg) ->
            WS.sendTextData conn ("Wrong announcement" :: Text)
          | any ($ client.name)
            [T.null, T.any isPunctuation, T.any isSpace] ->
                WS.sendTextData conn ("Name cannot " <>
                  "contain punctuation or whitespace, and " <>
                  "cannot be empty" :: Text)
          | clientExists client clients -> WS.sendTextData conn ("User already exists" :: Text)
          | otherwise -> flip finally disconnect $ do
              (s', s) <- atomically $ do
                s' <- readTVar state
                modifyTVar state $ addClient client
                s <- readTVar state
                pure (s', s)
              WS.sendTextData conn $
                  "Welcome! Users: " <>
                  T.intercalate ", " (map (.name) s.clients)
              broadcast (client.name <> " joinded") s'
              talk client state
          where
            prefix ="Hi! I am "
            client = Client { name = T.drop (T.length prefix) msg, conn }
            disconnect = do
              s <- atomically $ do
                s' <- readTVar state
                modifyTVar state $ \s -> removeClient client s
                pure s'
              broadcast (client.name <> "disconnected") s


talk :: Client -> TVar ServerState -> IO ()
talk c state = forever $ do
  msg <- WS.receiveData c.conn
  case msg of
    _ | msg == ":clients" -> do
          s <- readTVarIO state
          WS.sendTextData c.conn $ "All users: " <> T.intercalate ", " (map (.name) s.clients)
          pure ()
      | otherwise -> readTVarIO state >>= broadcast (c.name `mappend` ": " `mappend` msg)