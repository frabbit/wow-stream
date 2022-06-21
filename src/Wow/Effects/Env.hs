module Wow.Effects.Env where

import Prelude

import Polysemy (makeSem, Member, Embed, Sem, interpret, embed)
import Data.Kind (Type)
import qualified System.Environment as E

data Env (m::Type -> Type) t where
  GetEnv :: String -> Env m String

makeSem ''Env

envToIo :: (Member (Embed IO) r) => Sem (Env ': r) a -> Sem r a
envToIo = interpret $ \case
  GetEnv t -> embed $ E.getEnv t
