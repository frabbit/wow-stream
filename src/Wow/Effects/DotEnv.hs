module Wow.Effects.DotEnv where

import Prelude
import Polysemy (makeSem, Member, Embed, Sem, interpret, embed)
import Data.Kind (Type)
import Control.Monad (void)
import Configuration.Dotenv (loadFile, defaultConfig)

data DotEnv (m::Type->Type) a where
  LoadDotEnv :: DotEnv m ()

makeSem ''DotEnv

dotEnvToIo :: (Member (Embed IO) r) => Sem (DotEnv ': r) a -> Sem r a
dotEnvToIo = interpret $ \case
  LoadDotEnv -> embed @IO $ void $ loadFile defaultConfig
