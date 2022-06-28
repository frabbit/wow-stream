{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Wow.Effects.ServerApi where

import Wow.Prelude
import Polysemy (Sem, interpretH, Members, Member)
import Polysemy.Internal (send)
import Data.Kind (Type)
import Wow.Effects.ClientChannel (ClientChannel, sendMessage, ConnectionNotAvailableError)
import Polysemy.AtomicState (AtomicState, atomicModify)
import Wow.Data.ServerState (ServerState, setClientListening, setClientFilter)
import Veins.Control.Monad.VExceptT (VExceptT (VExceptT), runVExceptT, liftVExceptT)
import Wow.Data.ServerMessage (ServerMessage(SMAcknowledge))
import Control.Monad.Trans (lift)
import Wow.Data.Client (Client)
import Polysemy.Internal.Tactics (liftT)
import Veins.Data.VEither (VEither)

type ServerApi :: (Type -> Type) -> Type -> Type
data ServerApi m a where
  Listen :: Client -> ServerApi m (VEither '[ConnectionNotAvailableError] ())
  Unlisten :: Client -> ServerApi m (VEither '[ConnectionNotAvailableError] ())

listen :: (Member ServerApi r) => Client ->  VExceptT '[ConnectionNotAvailableError] (Sem r) ()
listen client = VExceptT $ send (Listen client)

unlisten :: (Member ServerApi r) => Client ->  VExceptT '[ConnectionNotAvailableError] (Sem r) ()
unlisten client = VExceptT $ send (Unlisten client)

interpretServerApi :: (Members [ClientChannel, AtomicState ServerState] r ) => Sem (ServerApi ': r) a -> Sem r a
interpretServerApi = interpretH $ \case
  Listen client -> do
    liftT . runVExceptT $ listenImpl client
  Unlisten client -> do
    liftT . runVExceptT $ unlistenImpl client

listenImpl :: (Members [ClientChannel, AtomicState ServerState] r) => Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
listenImpl c = do
  sendMessage c.clientId (SMAcknowledge "listen")
  lift $ atomicModify @ServerState (setClientListening c True)
  pure ()

unlistenImpl :: (Members [ClientChannel, AtomicState ServerState] r) => Client -> VExceptT '[ConnectionNotAvailableError] (Sem r) ()
unlistenImpl c = do
  liftVExceptT $ sendMessage c.clientId (SMAcknowledge "unlisten")
  lift $ atomicModify @ServerState $ setClientListening c False
  pure ()
