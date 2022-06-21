module Wow.Effects.Errors where

import Prelude

import Polysemy.Error ( Error, runError )
import Polysemy (Sem)


recover :: Sem (Error e ': r) a -> (e -> Sem r a) -> Sem r a
recover s f = do
  e <- runError s
  case e of
    Left l -> f l
    Right r -> pure r
