{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Wow.Websocket where

import Prelude
import Data.Text (Text)
import Control.Monad (forM_)
import Control.Monad.Cont (forever, MonadTrans (lift))
import Debug.Trace (traceShowM)
import Polysemy (Sem, Members)
import Wow.Effects.Finally (finally, Finally)
import Wow.Data.Command (Command (CmdGreeting, CmdClients, CmdListen, CmdFilter, CmdTalk, CmdUnlisten))
import Wow.Data.ClientId (ClientId)
import Wow.Effects.ClientChannel (receiveMessage, ClientChannel, sendMessage, ConnectionNotAvailableError, InvalidCommandError (InvalidCommandError))
import Wow.Data.ServerMessage (ServerMessage(SMAcknowledge, SMClientDisconnected, SMClients, SMClientJoined, SMUnexpectedCommand, SMWelcome, SMError, SMTalk), Error (ErrUsernameExists, ErrGreetingAlreadySucceded, ErrNotAuthenticated))
import Veins.Control.Monad.VExceptT (VExceptT (VExceptT), catchVExceptT, evalVExceptT, liftVExceptT, runVExceptT)
import Data.Function ((&))
import Wow.Data.ServerState (ServerState, addClient, nameExists, removeClient, setClientListening, setClientFilter)
import Wow.Data.Client (Client (..))
import Polysemy.AtomicState (AtomicState, atomicState, atomicGet, atomicModify)
import Wow.Effects.ServerApi (ServerApi, listen)

broadcastSilent :: (Members '[ClientChannel] r) => ServerMessage -> ServerState -> Sem r ()
broadcastSilent = broadcastSilentWhen (const True)

broadcastSilentWhen :: (Members '[ClientChannel] r) => (Client -> Bool) -> ServerMessage -> ServerState -> Sem r ()
broadcastSilentWhen f message s = do
  forM_ s.clients sendIf
  where
    sendIf c = if f c
      then sendMessage c.clientId (message) `catchVExceptT` (\(_::ConnectionNotAvailableError) -> pure ()) & evalVExceptT
      else pure ()

broadcast :: (Members '[ClientChannel] r) => ServerMessage -> ServerState -> Sem r ()
broadcast message s = do
  traceShowM message
  broadcastSilent message s

handleClient :: forall r. (Members '[ClientChannel, Finally, AtomicState ServerState, ServerApi ] r) => ClientId -> (Sem r) ()
handleClient clientId = evalVExceptT $ do
  msg <- receiveMessage clientId
  case msg of
    CmdGreeting n -> liftVExceptT $ greeting n clientId
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


greeting :: forall r . (Members '[Finally, ClientChannel, AtomicState ServerState, ServerApi] r) => Text -> ClientId -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
greeting n clientId = VExceptT $ flip finally (disconnect client) $ runVExceptT handleGreeting
  where
    handleGreeting :: VExceptT '[ConnectionNotAvailableError] (Sem r) ()
    handleGreeting = do
      res <- lift $ atomicState @ServerState $ \s' ->
        if nameExists n s' then
          (s', Nothing)
        else
          let newState = addClient client s' in
          (newState, Just (s', newState))
      case res of
        Nothing -> sendMessage clientId (SMError ErrUsernameExists)

        Just (s', s) -> do
          sendMessage clientId (SMWelcome (map (.name) s.clients))
          lift $ broadcast (SMClientJoined $ client.name) s'
          talk client
    client = Client { name = n, clientId, listening = False, tweetFilter = Nothing }

disconnect :: (Members '[ClientChannel, AtomicState ServerState] r) => Client -> (Sem r) ()
disconnect client = do
  traceShowM ("disconnect"::Text)
  s <- atomicState @ServerState $ \s -> let sn = removeClient client s in (sn, sn)
  traceShowM ("disconnect broadcast..."::Text)
  traceShowM s
  broadcast (SMClientDisconnected $ client.name) s

talk :: (Members [ClientChannel, AtomicState ServerState, ServerApi] r) => Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
talk c = forever $ do
  cmd <- receiveMessage c.clientId
  case cmd of
    CmdClients -> do
          s <- lift $ atomicGet @ServerState
          liftVExceptT $ sendMessage c.clientId (SMClients $ map (.name) s.clients)
          pure ()
    CmdListen -> do
          liftVExceptT $ listen c
    CmdFilter f -> do
          liftVExceptT $ sendMessage c.clientId (SMAcknowledge "filter")
          lift $ atomicModify @ServerState $ setClientFilter c (Just f)
          pure ()
    CmdUnlisten -> do
          liftVExceptT $ sendMessage c.clientId (SMAcknowledge "unlisten")
          lift $ atomicModify @ServerState $ setClientListening c False
          pure ()
    CmdTalk msg ->
          (lift $ atomicGet @ServerState) >>= (lift . broadcast (SMTalk c.name msg))
    CmdGreeting _ ->
          liftVExceptT $ sendMessage c.clientId (SMError ErrGreetingAlreadySucceded)

  `catchVExceptT` (\(InvalidCommandError t) -> sendMessage c.clientId (SMUnexpectedCommand t))