module Veins.Data.Variant.EitherUtil where

import Veins.Prelude
import Veins.Data.Variant

catchEitherVariant :: forall e errs errout a . (RemoveVariant e errs errout) => Either (Variant errs) a -> (e -> Either (Variant errout) a) -> Either (Variant errout) a
catchEitherVariant (Left v0) f = case remove @e v0 of
  Right v -> f v
  Left tail' -> Left tail'
catchEitherVariant (Right r) _ = Right r