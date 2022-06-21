{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Veins.Data.VEither where

import Veins.Prelude
import Veins.Data.Variant ( Variant, RemoveVariant, liftVariant, inject, InjectVariant )
import Veins.Data.Variant.EitherUtil (catchEitherVariant)
import Data.Either.Extra (mapLeft)

newtype VEither lefts right = VEither {unVEither :: Either (Variant lefts) right } deriving newtype (Monad, Applicative, Functor)

deriving newtype instance (Show (Variant left), Show right) => Show (VEither left right)

deriving newtype instance (Eq (Variant left), Eq right) => Eq (VEither left right)

deriving newtype instance (Ord (Variant left), Ord right) => Ord (VEither left right)

catchVEither :: RemoveVariant e errs errs2 => VEither errs a -> (e -> VEither errs2 a) -> VEither errs2 a
catchVEither v f = VEither $ catchEitherVariant v.unVEither (unVEither . f)

throwVEither :: (InjectVariant e errs) => e -> VEither errs a
throwVEither = mkVLeft . inject

mkVLeft :: Variant lefts -> VEither lefts right
mkVLeft = VEither . Left

liftVEither :: _ => VEither errs a -> VEither errout a
liftVEither = VEither . mapLeft liftVariant . unVEither

evalVEither :: VEither '[] a -> a
evalVEither e= case unVEither e of
  Left _ -> error "impossible"
  Right v -> v