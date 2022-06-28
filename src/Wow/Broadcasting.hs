{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use readTVarIO" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Wow.Broadcasting where

import Prelude hiding (filter)
import Control.Monad (forM_)
import Debug.Trace (traceShowM)
import Polysemy (Sem, Members)
import Wow.Effects.ClientChannel (ClientChannel, sendMessage, ConnectionNotAvailableError)
import Wow.Data.ServerMessage (ServerMessage)
import Veins.Control.Monad.VExceptT (catchVExceptT, evalVExceptT)
import Data.Function ((&))
import Wow.Data.ServerState (ServerState)
import Wow.Data.Client (Client (..))

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