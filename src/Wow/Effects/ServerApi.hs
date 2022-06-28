{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE StandaloneDeriving #-}
module Wow.Effects.ServerApi where

import Wow.Prelude
import Polysemy (Sem, interpretH, Members, Member)
import Polysemy.Internal (send)
import Data.Kind (Type)
import Wow.Effects.ClientChannel (ClientChannel, sendMessage, ConnectionNotAvailableError)
import Polysemy.AtomicState (AtomicState, atomicModify, atomicGet)
import Wow.Data.ServerState (ServerState, setClientListening, setClientFilter)
import Veins.Control.Monad.VExceptT (VExceptT (VExceptT), runVExceptT, liftVExceptT)
import Wow.Data.ServerMessage (ServerMessage(SMAcknowledge, SMClients, SMTalk))
import Control.Monad.Trans (lift)
import Wow.Data.Client (Client)
import Polysemy.Internal.Tactics (liftT)
import Veins.Data.VEither (VEither)
import Wow.Broadcasting (broadcast)

type ServerApi :: (Type -> Type) -> Type -> Type
data ServerApi m a where
  Listen :: Client -> ServerApi m (VEither '[ConnectionNotAvailableError] ())
  Unlisten :: Client -> ServerApi m (VEither '[ConnectionNotAvailableError] ())
  Filter :: Text -> Client -> ServerApi m (VEither '[ConnectionNotAvailableError] ())
  ListClients :: Client -> ServerApi m (VEither '[ConnectionNotAvailableError] ())
  Talk  :: Text -> Client -> ServerApi m (VEither '[ConnectionNotAvailableError] ())

deriving instance Show (ServerApi m a)


listClients :: (Member ServerApi r) => Client ->  VExceptT '[ConnectionNotAvailableError] (Sem r) ()
listClients client = VExceptT $ send (ListClients client)

listen :: (Member ServerApi r) => Client ->  VExceptT '[ConnectionNotAvailableError] (Sem r) ()
listen client = VExceptT $ send (Listen client)

unlisten :: (Member ServerApi r) => Client ->  VExceptT '[ConnectionNotAvailableError] (Sem r) ()
unlisten client = VExceptT $ send (Unlisten client)

filter :: (Member ServerApi r) => Text -> Client ->  VExceptT '[ConnectionNotAvailableError] (Sem r) ()
filter f client = VExceptT $ send (Filter f client)

talk :: (Member ServerApi r) => Text -> Client ->  VExceptT '[ConnectionNotAvailableError] (Sem r) ()
talk msg client = VExceptT $ send (Talk msg client)

interpretServerApi :: (Members [ClientChannel, AtomicState ServerState] r ) => Sem (ServerApi ': r) a -> Sem r a
interpretServerApi = interpretH $ \case
  Listen client -> do
    liftT . runVExceptT $ listenImpl client
  Unlisten client -> do
    liftT . runVExceptT $ unlistenImpl client
  Filter f client -> do
    liftT . runVExceptT $ filterImpl f client
  ListClients client -> do
    liftT . runVExceptT $ listClientsImpl client
  Talk message client -> do
    liftT . runVExceptT $ talkImpl message client

listenImpl :: (Members [ClientChannel, AtomicState ServerState] r) => Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
listenImpl c = do
  sendMessage c.clientId (SMAcknowledge "listen")
  lift $ atomicModify @ServerState (setClientListening c True)
  pure ()

listClientsImpl :: (Members [ClientChannel, AtomicState ServerState] r) => Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
listClientsImpl c = do
  s <- lift $ atomicGet @ServerState
  liftVExceptT $ sendMessage c.clientId (SMClients $ map (.name) s.clients)
  pure ()

unlistenImpl :: (Members [ClientChannel, AtomicState ServerState] r) => Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
unlistenImpl c = do
  liftVExceptT $ sendMessage c.clientId (SMAcknowledge "unlisten")
  lift $ atomicModify @ServerState $ setClientListening c False
  pure ()

filterImpl :: (Members [ClientChannel, AtomicState ServerState] r) => Text -> Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
filterImpl f c = do
  liftVExceptT $ sendMessage c.clientId (SMAcknowledge "filter")
  lift $ atomicModify @ServerState $ setClientFilter c (Just f)
  pure ()

talkImpl :: (Members [ClientChannel, AtomicState ServerState] r) => Text -> Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
talkImpl msg c =
  (lift $ atomicGet @ServerState) >>= lift . broadcast (SMTalk c.name msg)
