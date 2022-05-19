module Data.Mul where

import Prelude

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

data MulF :: Type -> Type
data MulF a = MulF a a

type Mul :: Row (Type -> Type) -> Row (Type -> Type)
type Mul r = (mul ∷ MulF | r)

_mul = Proxy ∷ _ "mul"

mul ∷ forall r. EADT (Mul + r) -> EADT (Mul + r) -> EADT (Mul + r)
mul x y = L.review _MulF (Tuple x y)

derive instance Functor MulF
derive instance Generic (MulF a) _

instance Show a => Show (MulF a) where
  show v = genericShow v

instance Foldable MulF where
  foldl f z (MulF a b) = f (f z a) b
  foldr f z (MulF a b) = f a (f b z)
  foldMap f (MulF a b) = f a <> f b

instance Traversable MulF where
  sequence = sequenceDefault
  traverse f (MulF a b) = MulF <$> f a <*> f b

class AsMulF s a | s -> a where
  _MulF ∷ L.Prism' s (Tuple a a)

instance AsMulF (MulF a) a where
  _MulF = L.prism' (uncurry MulF) (\(MulF a b) -> Just (Tuple a b))

else instance (Functor f, AsMulF (f a) a, TypeEquals (VF.VariantF ( mul :: f | tail ) a) (VF.VariantF row a)) => AsMulF (VF.VariantF row a) a where
  _MulF = dimap TE.from TE.to <<< _VariantF _mul <<< _MulF

else instance (Functor f, AsMulF (f (Mu f)) a) => AsMulF (Mu f) a where
  _MulF = L.re _Mu <<< _MulF