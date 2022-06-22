{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Wow.Websocket where

import Prelude
import qualified Network.WebSockets as WS
import Data.Text (Text)
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
import Wow.Effects.ClientChannel (receiveMessage, ClientChannel, sendMessage, ConnectionNotAvailableError, InvalidCommandError (InvalidCommandError))
import Wow.Data.ServerMessage (ServerMessage(SMAcknowledge, SMClientDisconnected, SMClients, SMClientJoined, SMUnexpectedCommand, SMWelcome, SMError, SMTalk), Error (ErrUsernameExists, ErrGreetingAlreadySucceded, ErrNotAuthenticated))
import Veins.Control.Monad.VExceptT (VExceptT (VExceptT), catchVExceptT, evalVExceptT, liftVExceptT, runVExceptT)
import Data.Function ((&))
import Wow.Data.ServerState (ServerState, addClient, nameExists, removeClient, setClientListening, setClientFilter)
import Wow.Data.Client (Client (..))


broadcastSilent :: (Members '[ClientChannel] r) => ServerMessage -> ServerState -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
broadcastSilent message s = do
  forM_ s.clients $ \c -> sendMessage c.clientId message

broadcastSilentWhen :: (Members '[ClientChannel] r) => (Client -> Bool) -> ServerMessage -> ServerState -> Sem r ()
broadcastSilentWhen f message s = do
  forM_ s.clients sendIf
  where
    sendIf c = if f c
      then sendMessage c.clientId (message) `catchVExceptT` (\(_::ConnectionNotAvailableError) -> pure ()) & evalVExceptT
      else pure ()

broadcast :: (Member ClientChannel r) => ServerMessage -> ServerState -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
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
    _ -> liftVExceptT $ sendMessage clientId (SMError ErrNotAuthenticated)
  `catchVExceptT` (
    \(_::ConnectionNotAvailableError) -> do
      traceShowM ("Conn not available"::Text)
      pure ()
    )
  `catchVExceptT` (
    \(_::InvalidCommandError) -> do
      traceShowM ("Invalid Command"::Text)
      pure ()
    )


greeting :: forall r . (Members '[Finally, STM, ClientChannel] r) => Text -> ClientId -> TVar ServerState -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
greeting n clientId state = VExceptT $ flip finally (disconnect state client) $ runVExceptT handleGreeting
  where
    handleGreeting :: VExceptT '[ConnectionNotAvailableError] (Sem r) ()
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
        Nothing -> sendMessage clientId (SMError ErrUsernameExists)
        Just (s', s) -> do
          sendMessage clientId (SMWelcome (map (.name) s.clients))
          broadcast (SMClientJoined $ client.name) s'
          talk client state
    client = Client { name = n, clientId, listening = False, tweetFilter = Nothing }

disconnect :: (Members '[ClientChannel, STM] r) => TVar ServerState -> Client -> (Sem r) ()
disconnect state client = evalVExceptT $ do
  traceShowM ("disconnect"::Text)
  s <- lift $ atomically $ do
    modifyTVar state $ \s -> removeClient client s
    readTVar state
  traceShowM ("disconnect broadcast..."::Text)
  traceShowM s
  broadcast (SMClientDisconnected $ client.name) s
  `catchVExceptT` (\(_::ConnectionNotAvailableError) -> do
    traceShowM ("Conn not available"::Text)
    pure ())

talk :: (Members [ClientChannel, STM] r) => Client -> TVar ServerState -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
talk c state = forever $ do
  cmd <- receiveMessage c.clientId
  case cmd of
    CmdClients -> do
          s <- lift $ atomically $ readTVar state
          liftVExceptT $ sendMessage c.clientId (SMClients $ map (.name) s.clients)
          pure ()
    CmdListen -> do
          liftVExceptT $ sendMessage c.clientId (SMAcknowledge "listen")
          lift $ atomically $ modifyTVar state $ setClientListening c True
          pure ()
    CmdFilter f -> do
          liftVExceptT $ sendMessage c.clientId (SMAcknowledge "filter")
          lift $ atomically $ modifyTVar state $ setClientFilter c (Just f)
          pure ()
    CmdUnlisten -> do
          liftVExceptT $ sendMessage c.clientId (SMAcknowledge "unlisten")
          lift $ atomically $ modifyTVar state $ setClientListening c False
          pure ()
    CmdTalk msg ->
          (lift $ atomically $ readTVar state) >>= (liftVExceptT . broadcast (SMTalk c.name msg))
    CmdGreeting _ ->
          liftVExceptT $ sendMessage c.clientId (SMError ErrGreetingAlreadySucceded)

  `catchVExceptT` (\(InvalidCommandError t) -> sendMessage c.clientId (SMUnexpectedCommand t))