module Wow.Effects.Finally where

import Prelude

import Polysemy (Embed, Member, Sem, interpretH, runT, embed, makeSem, runTSimple)
import qualified Control.Exception.Base as E

data Finally m a where
  Finally :: m a -> m b -> Finally m a

makeSem ''Finally

finallyToIo :: (Member (Embed IO) r) => (forall x . Sem (Finally ': r) x -> IO x) -> Sem (Finally ': r) a -> Sem r a
finallyToIo nt = interpretH $ \case
  Finally a b -> do
    a' <- runT a
    b' <- runT b
    let res = E.finally (nt a') (nt b')
    embed res

finallyPure :: forall r a . Sem (Finally ': r) a -> Sem r a
finallyPure = interpretH $ \case
  Finally a b -> do
    a' <- runTSimple a
    runTSimple b
    pure a'
