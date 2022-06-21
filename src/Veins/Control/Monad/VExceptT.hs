{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <=<" #-}
module Veins.Control.Monad.VExceptT where

import Control.Applicative (Applicative (liftA2))
import Control.Category ((>>>))
import Veins.Data.VEither
import Veins.Data.Variant (InjectVariant, LiftVariant, RemoveVariant, inject, remove)
import Veins.Prelude
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)

newtype VExceptT errs m a = VExceptT {runVExceptT :: m (VEither errs a)}

instance (Functor m) => Functor (VExceptT errs m) where
  fmap f = VExceptT . fmap (fmap f) . runVExceptT

instance (Applicative m) => Applicative (VExceptT errs m) where
  pure = VExceptT . pure . pure
  liftA2 :: forall a b c. (a -> b -> c) -> VExceptT errs m a -> VExceptT errs m b -> VExceptT errs m c
  liftA2 f = \a b -> VExceptT $ f' (runVExceptT a) (runVExceptT b)
    where
      f' :: m (VEither err a) -> m (VEither err b) -> m (VEither err c)
      f' = liftA2 . liftA2 $ f

instance (Monad m) => Monad (VExceptT errs m) where
  a >>= f = VExceptT $ do
    VEither a' <- runVExceptT a
    case a' of
      Left errs -> pure . mkVLeft $ errs
      Right v -> runVExceptT $ f v

instance MonadTrans (VExceptT errs) where
  lift :: (Monad m) => m a -> VExceptT errs m a
  lift = VExceptT . fmap pure

liftVExceptT :: (Functor m, LiftVariant errs1 errs2) => VExceptT errs1 m a -> VExceptT errs2 m a
liftVExceptT = VExceptT . fmap liftVEither . runVExceptT

throwVExceptT :: (Applicative m, (InjectVariant e errs)) => e -> VExceptT errs m a
throwVExceptT = VExceptT . pure . mkVLeft . inject

evalVExceptT :: (Functor m) => VExceptT '[] m a -> m a
evalVExceptT = fmap evalVEither . runVExceptT

catchVExceptT ::
  forall m errs1 errs2 a e.
  ( RemoveVariant e errs1 errs2,
    Monad m
  ) =>
  VExceptT errs1 m a ->
  (e -> VExceptT errs2 m a) ->
  VExceptT errs2 m a
catchVExceptT e f = VExceptT . f' . runVExceptT $ e
  where
    f' :: m (VEither errs1 a) -> m (VEither errs2 a)
    f' x = x >>= (unVEither >>> case')
    case' = \case
      Left errs -> case remove @e errs of
        Left errs1 -> pure . mkVLeft $ errs1
        Right v -> runVExceptT $ f v
      Right r -> pure . pure $ r
