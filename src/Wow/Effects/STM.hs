{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Wow.Effects.STM where

import qualified GHC.Conc as C
import Wow.Prelude
import Polysemy (makeSem, Member, Embed, Sem, embed, interpret)
import Data.Kind (Type)

data STM (m::Type -> Type) a where
  Atomically :: C.STM a -> STM m a

makeSem ''STM

stmToIo :: (Member (Embed IO) r) => Sem (STM ': r) a -> Sem r a
stmToIo = interpret $ \case
  Atomically a -> do
    embed $ C.atomically a