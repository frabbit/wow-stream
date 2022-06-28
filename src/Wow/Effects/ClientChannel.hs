{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Wow.Effects.ClientChannel where

import Wow.Prelude
import Wow.Data.ServerMessage (ServerMessage, toText)
import Polysemy (Sem, reinterpretH, Member, Members, pureT, interpretH)
import Wow.Effects.WebSocket (WebSocket, sendTextData, receiveData)
import Data.Kind (Type)
import Polysemy.Internal.Tactics (liftT)
import Network.WebSockets (Connection, ConnectionException (ConnectionClosed, CloseRequest))
import Wow.Data.ClientId (ClientId)
import Wow.Data.Command (Command, parseCommand)
import Polysemy.Input (Input, input)
import qualified Data.Map.Strict as M
import Polysemy.Internal (send)
import Conduit (MonadTrans(lift))
import Polysemy.AtomicState (AtomicState, atomicModify')
import Debug.Trace (traceShowM)
import Control.Exception (throw)
import Veins.Data.VEither (VEither)
import Veins.Control.Monad.VExceptT (VExceptT (VExceptT), catchVExceptT, evalVExceptT, runVExceptT, throwVExceptT)

type ClientLookup = M.Map ClientId Connection

data ConnectionNotAvailableError = ConnectionNotAvailableError
data InvalidCommandError = InvalidCommandError Text

data ClientChannel (m::Type->Type) a where
  SendMessage :: ClientId -> ServerMessage -> ClientChannel m (VEither '[ConnectionNotAvailableError] ())
  ReceiveMessage :: ClientId -> ClientChannel m (VEither '[ConnectionNotAvailableError, InvalidCommandError] Command)

-- expose possible errors via smart constructors
sendMessage :: (Member ClientChannel r) => ClientId -> ServerMessage -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
sendMessage cId msg = VExceptT $ send (SendMessage cId msg)

receiveMessage :: (Member ClientChannel r) => ClientId -> VExceptT '[ConnectionNotAvailableError, InvalidCommandError] (Sem r) Command
receiveMessage = VExceptT . send . ReceiveMessage

sendMessageOrIgnore :: ClientId -> ServerMessage -> Sem (ClientChannel : r) ()
sendMessageOrIgnore id' msg = evalVExceptT $ sendMessage id' msg
  `catchVExceptT` (\(_::ConnectionNotAvailableError) -> pure ())

interpretClientChannel :: forall r a . (Members '[AtomicState ClientLookup, Input ClientLookup] r) => Sem (ClientChannel ': r) a -> Sem (WebSocket ': r) a
interpretClientChannel = reinterpretH $ \case
  SendMessage clientId sm -> liftT $ runVExceptT $ do
    d <- lift $ input @ClientLookup
    let conn = M.lookup clientId d
    conn' <- maybe (throwVExceptT ConnectionNotAvailableError) pure conn
    let t = toText sm
    lift $ sendTextData conn' t
  ReceiveMessage clientId -> liftT $ runVExceptT $ do
    d <- lift $ input @ClientLookup
    let conn = M.lookup clientId d
    traceShowM (M.keys d)
    conn' <- maybe (throwVExceptT ConnectionNotAvailableError) pure conn

    res <- lift $ do
      receiveData conn'
    let
      removeClient :: VExceptT errs (Sem (WebSocket ': r)) ()
      removeClient = do
        lift $ atomicModify' @ClientLookup (M.delete clientId)

    t <- case res of
      Left e@ConnectionClosed {} -> do
        traceShowM ("ConnectionClosed occured"::Text)
        removeClient
        throw e
      Left e@CloseRequest {} -> do
        traceShowM ("CloseRequest occured"::Text)
        removeClient
        traceShowM $ M.keys d
        throw e
      Left e -> do
        traceShowM ("error occured" <> show e)
        throw e
      Right t ->
        pure t


    either (const (throwVExceptT $ InvalidCommandError t)) pure . parseCommand $ t

