module Data.Val where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Functor.Mu (Mu)
import Data.Functor.Variant as VF
import Data.Generic.Rep (class Generic)
import Data.Lens as L
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault)
import EADT (EADT, _Mu, _VariantF)
import Type.Equality (class TypeEquals)
import Type.Equality as TE
import Type.Proxy (Proxy(..))

newtype ValF :: Type -> Type
newtype ValF a = ValF Int

type Val :: Row (Type -> Type) -> Row (Type -> Type)
type Val r = (val ∷ ValF | r)

_val = Proxy ∷ _ "val"

val ∷ forall r. Int -> EADT (Val r)
val = L.review _ValF

derive instance Functor ValF
derive instance Generic (ValF a) _

instance Eq a => Eq (ValF a) where
  eq a b = genericEq a b

instance Show a => Show (ValF a) where
  show v = genericShow v

instance Foldable ValF where
  foldl _ z (ValF _) = z
  foldr _ z (ValF _) = z
  foldMap _ _ = mempty

instance Traversable ValF where
  sequence = sequenceDefault
  traverse _ (ValF a) = pure (ValF a)

class AsValF :: forall k. Type -> k -> Constraint
class AsValF s a | s -> a where
  _ValF ∷ L.Prism' s Int

instance AsValF (ValF a) a where
  _ValF = L.prism' ValF (\(ValF a) -> Just a)

else instance (Functor f, AsValF (f a) a, TypeEquals (VF.VariantF ( val :: f | tail ) a) (VF.VariantF row a)) => AsValF (VF.VariantF row a) a where
  _ValF = dimap TE.from TE.to <<< _VariantF _val <<< _ValF

else instance (Functor f, AsValF (f (Mu f)) a) => AsValF (Mu f) a where
  _ValF = L.re _Mu <<< _ValF