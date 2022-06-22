{-# LANGUAGE AllowAmbiguousTypes #-}
module Veins.Data.Variant (Variant, inject, project, remove, liftVariant, toMaybe, RemoveVariant, LiftVariant, InjectVariant) where
import Veins.Prelude
import Data.Type.Equality (type (==))
import Data.Kind (Type)
import Data.Either.Extra (mapLeft)

data Variant es where
  VLeft :: a -> Variant (a ': es)
  VRight :: Variant es -> Variant (a ': es)

instance Eq (Variant '[]) where
  (==) _ _ = True

instance (Eq a, Eq (Variant es)) => Eq (Variant (a ': es)) where
  VLeft a1 == VLeft a2 = a1 == a2
  VRight r1 == VRight r2 = r1 == r2
  _ == _ = False

instance Ord (Variant '[]) where
  (compare) _ _ = EQ

instance (Ord a, Ord (Variant es)) => Ord (Variant (a ': es)) where
  VLeft a1 `compare` VLeft a2 = a1 `compare` a2
  VRight r1 `compare` VRight r2 = r1 `compare` r2
  VLeft _ `compare` VRight _ = LT
  VRight _ `compare` VLeft _ = GT


instance Show (Variant '[]) where
  show _ = error "unexpected"

instance (Show a, Show (Variant es)) => Show (Variant (a ': es)) where
  show (VLeft l) = "VLeft " <> show l
  show (VRight r) = "VRight (" <> show r <> ")"

class LiftVariant as es where
  liftVariant :: Variant as -> Variant es

instance LiftVariant '[] out where
  liftVariant _ = error "unexpected"

instance (InjectVariant a out) => LiftVariant (a ': '[]) out where
  liftVariant (VLeft l) = inject l
  liftVariant (VRight _) = error "unexpected"

instance (LiftVariant (b : es1) out, InjectVariant a out) => LiftVariant (a ': b : es1) out where
  liftVariant (VLeft l) = inject l
  liftVariant (VRight r) = liftVariant r

type family Remove e errs :: [Type] where
  Remove e (e ': es) = es
  Remove e (a ': es) = (a ': Remove e es)

class RemoveVariant e errs out | e errs -> out where
  remove :: Variant errs -> Either (Variant out) e

instance (isEqual ~ (a == b), isEmpty ~ (es == '[]), RemoveVariantCase isEqual isEmpty a (b ': es) out) => RemoveVariant a (b ': es) out where
  remove = removeCase @isEqual @isEmpty @a

class RemoveVariantCase (equal::Bool) (isEmpty::Bool) e (es::[Type]) out | equal e es -> out where
  removeCase :: Variant es -> Either (Variant out) e

instance RemoveVariantCase 'True _ignore a (a ': es) es where
  removeCase (VLeft v) = Right v
  removeCase (VRight r) = Left r

instance RemoveVariantCase 'False 'True a (b ': '[]) (b ': '[]) where
  removeCase = Left

instance (RemoveVariant a es out1, LiftVariant out1 (b ': out1), InjectVariant b (b ': out1)) => RemoveVariantCase 'False 'False a (b ': es) (b ': out1) where
  removeCase (VLeft v) = Left $ inject v
  removeCase (VRight r) = mapLeft liftVariant (remove @a r)

class Project a es where
  project :: Variant es -> Maybe a

instance (isEqual ~ (a == b), ProjectCase isEqual a (b ': xs)) => Project a (b ': xs) where
  project = projectCase @isEqual

class ProjectCase (eq :: Bool) a es where
  projectCase :: Variant es -> Maybe a

instance ProjectCase 'True a (a ': xs) where
  projectCase (VLeft v) = Just v
  projectCase _ = Nothing

instance (Project a xs) => ProjectCase 'False a (b ': xs) where
  projectCase (VLeft _) = Nothing
  projectCase (VRight r) = project r

class InjectVariant a es where
  inject :: a -> Variant es

instance (isEqual ~ (a == b), InjectVariantCase isEqual a (b ': xs)) => InjectVariant a (b ': xs) where
  inject = injectCase @isEqual

class InjectVariantCase (eq :: Bool) a es where
  injectCase :: a -> Variant es

instance InjectVariantCase 'True a (a ': xs) where
  injectCase = VLeft

instance (InjectVariant a xs) => InjectVariantCase 'False a (b ': xs) where
  injectCase = VRight . inject

toMaybe :: (Project a es) => (Variant es -> Maybe a)
toMaybe = project