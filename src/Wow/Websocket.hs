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
import Control.Monad.Cont (forever, MonadTrans (lift))
import Debug.Trace (traceShowM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Polysemy (Sem, Member, Members)
import Wow.Effects.Finally (finally, Finally)
import Wow.Effects.STM (STM, atomically)
import Wow.Data.Command (Command (CmdGreeting, CmdClients, CmdListen, CmdFilter, CmdTalk, CmdUnlisten))
import Wow.Data.ClientId (ClientId)
import Wow.Effects.ClientChannel (receiveMessage, ClientChannel, sendMessage, ConnectionNotAvailableError, InvalidCommandError)
import Wow.Data.ServerMessage (ServerMessage(SMSimpleText))
import Veins.Control.Monad.VExceptT (VExceptT (VExceptT), catchVExceptT, evalVExceptT, liftVExceptT, runVExceptT)
import Data.Function ((&))

data Client = Client {
  name :: Text,
  clientId:: ClientId,
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

broadcastSilent :: (Members '[ClientChannel] r) => Text -> ServerState -> VExceptT _ (Sem r) ()
broadcastSilent message s = do
  forM_ s.clients $ \c -> sendMessage c.clientId (SMSimpleText message)

broadcastSilentWhen :: (Members '[ClientChannel] r) => (Client -> Bool) -> Text -> ServerState -> Sem r ()
broadcastSilentWhen f message s = do
  forM_ s.clients sendIf
  where
    sendIf c = if f c
      then sendMessage c.clientId (SMSimpleText message) `catchVExceptT` (\(_::ConnectionNotAvailableError) -> pure ()) & evalVExceptT
      else pure ()

broadcast :: (_) => Text -> ServerState -> VExceptT _ (Sem r) ()
broadcast message s = do
  traceShowM message
  broadcastSilent message s

withPingThreadUnliftIO :: (MonadIO m, MonadUnliftIO m) => WS.Connection -> Int -> m () -> m a -> m a
withPingThreadUnliftIO conn interval pingAction appAction = do
  pingAction' <- toIO pingAction
  appAction' <- toIO appAction
  liftIO $ WS.withPingThread conn interval pingAction' appAction'

handleClient :: forall r. (Member ClientChannel r, Member STM r, Member Finally r) => TVar ServerState -> ClientId -> (Sem r) ()
handleClient state clientId = evalVExceptT $ do
  msg <- receiveMessage clientId
  case msg of
    CmdGreeting n -> liftVExceptT $ greeting n clientId state
    _ -> liftVExceptT $ sendMessage clientId (SMSimpleText "Unexpected command")
  `catchVExceptT` (\(_::ConnectionNotAvailableError) -> do
    traceShowM "Conn not available"
    pure ())
  `catchVExceptT` (\(_::InvalidCommandError) -> do
    traceShowM "Invalid Command"
    pure ())


greeting :: forall r . (Members '[Finally, STM, ClientChannel] r) => _ -> _ -> _ -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
greeting n clientId state = VExceptT $ flip finally (evalVExceptT $ disconnect state client) $ runVExceptT handleGreeting
  where
    handleGreeting :: VExceptT _ (Sem r) ()
    handleGreeting = do
      res <- lift $ atomically $ do
        s' <- readTVar state
        if nameExists n s' then
          pure Nothing
        else do
          modifyTVar state $ addClient client
          s <- readTVar state
          pure $ Just (s', s)
      case res of
        Nothing -> sendMessage clientId (SMSimpleText "User already exists")
        Just (s', s) -> do
          sendMessage clientId (SMSimpleText $ "Welcome! Users: " <> T.intercalate ", " (map (.name) s.clients))
          broadcast (client.name <> " joined") s'
          talk client state
    client = Client { name = n, clientId, listening = False, tweetFilter = Nothing }

disconnect :: (Members '[ClientChannel, STM] r) => _ -> _ -> VExceptT '[] (Sem r) ()
disconnect state client = do
  traceShowM ("disconnect"::Text)
  s <- lift $ atomically $ do
    modifyTVar state $ \s -> removeClient client s
    readTVar state
  traceShowM ("disconnect broadcast..."::Text)
  traceShowM s
  broadcast (client.name <> " disconnected") s
  `catchVExceptT` (\(_::ConnectionNotAvailableError) -> do
    traceShowM "Conn not available"
    pure ())

talk :: (Members [ClientChannel, STM] r) => Client -> TVar ServerState -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
talk c state = forever $ do
  cmd <- receiveMessage c.clientId
  case cmd of
    CmdClients -> do
          s <- lift $ atomically $ readTVar state
          liftVExceptT $ sendMessage c.clientId $ (SMSimpleText $ "All users: " <> T.intercalate ", " (map (.name) s.clients))
          pure ()
    CmdListen -> do
          liftVExceptT $ sendMessage c.clientId (SMSimpleText "listen acknowledged.")
          lift $ atomically $ modifyTVar state $ setClientListening c True
          pure ()
    CmdFilter f -> do
          liftVExceptT $ sendMessage c.clientId (SMSimpleText "filter acknowledged.")
          lift $ atomically $ modifyTVar state $ setClientFilter c (Just f)
          pure ()
    CmdUnlisten -> do
          liftVExceptT $ sendMessage c.clientId (SMSimpleText "unlisten acknowledged.")
          lift $ atomically $ modifyTVar state $ setClientListening c False
          pure ()
    CmdTalk msg ->
          (lift $ atomically $ readTVar state) >>= (liftVExceptT . broadcast (c.name `mappend` ": " `mappend` msg))
    CmdGreeting _ ->
          liftVExceptT $ sendMessage c.clientId (SMSimpleText "Greeting already succeeded")

  `catchVExceptT` (\(_::InvalidCommandError) -> sendMessage c.clientId (SMSimpleText "Unexpected command"))