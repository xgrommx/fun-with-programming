module Data.Add where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (class Foldable)
import Data.Functor.Mu (Mu)
import Data.Functor.Variant as VF
import Data.Generic.Rep (class Generic)
import Data.Lens as L
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.Tuple (Tuple(..), uncurry)
import EADT (type (+), EADT, _Mu, _VariantF)
import Type.Equality (class TypeEquals)
import Type.Equality as TE
import Type.Proxy (Proxy(..))

data AddF :: Type -> Type
data AddF a = AddF a a

type Add :: Row (Type -> Type) -> Row (Type -> Type)
type Add r = (add ∷ AddF | r)

_add = Proxy ∷ _ "add"

add ∷ forall r. EADT (Add + r) -> EADT (Add + r) -> EADT (Add + r)
add x y = L.review _AddF (Tuple x y)

derive instance Functor AddF
derive instance Generic (AddF a) _

instance Show a => Show (AddF a) where
  show v = genericShow v

instance Foldable AddF where
  foldl f z (AddF a b) = f (f z a) b
  foldr f z (AddF a b) = f a (f b z)
  foldMap f (AddF a b) = f a <> f b

instance Traversable AddF where
  sequence = sequenceDefault
  traverse f (AddF a b) = lift2 AddF (f a) (f b)

--- classy prisms

class AsAddF (s :: Type) (a :: Type) | s -> a where
  _AddF ∷ L.Prism' s (Tuple a a)

instance AsAddF (AddF a) a where
  _AddF = L.prism' (uncurry AddF) (\(AddF a b) -> Just (Tuple a b))

else instance (Functor f, AsAddF (f a) a, TypeEquals (VF.VariantF ( add :: f | tail ) a) (VF.VariantF row a)) => AsAddF (VF.VariantF row a) a where
  _AddF = dimap TE.from TE.to <<< _VariantF _add <<< _AddF

else instance (Functor f, AsAddF (f (Mu f)) a) => AsAddF (Mu f) a where
  _AddF = L.re _Mu <<< _AddF