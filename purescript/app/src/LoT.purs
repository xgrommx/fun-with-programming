module LoT where

import Prelude

import Data.Either (Either)
import Type.Proxy (Proxy(..))

infixr 5 type LotN as :&&:

data LoT :: forall k. k -> Type
data LoT k

foreign import data LoT0 :: forall k. LoT k
foreign import data LotN :: forall k ks. k -> LoT ks -> LoT (k -> ks)

class HeadL :: forall k1 k2. LoT (k1 -> k2) -> k1 -> Constraint
class HeadL l h | l -> h, h -> l

instance HeadL (a :&&: as) a

class TailL :: forall k1 k2. LoT (k1 -> k2) -> LoT k2 -> Constraint
class TailL l t | l -> t, t -> l

instance TailL (a :&&: as) as

class ApplyL :: forall k. k -> LoT k -> Type -> Constraint
class ApplyL x l t | l x -> t

instance ApplyL f LoT0 f
else instance (HeadL as h, TailL as t, ApplyL (f h) t r) => ApplyL f as r

proxyApplyL :: forall x l t. ApplyL x l t => Proxy x -> Proxy l -> Proxy t
proxyApplyL _ _ = Proxy

e1 ∷ Proxy (Either Int Boolean)
e1 = proxyApplyL (Proxy :: _ Either) (Proxy :: _ (Int :&&: Boolean :&&: LoT0))