module EADT where

import Prelude

import Control.Comonad.Cofree (Cofree, head, (:<))
import Data.Functor.Mu (Mu, roll, unroll)
import Data.Functor.Variant as VF
import Data.Lens as L
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Matryoshka (class Recursive, Algebra, CoalgebraM, cata)
import Prim.Row as Row
import Type.Proxy (Proxy)

type RowApply :: forall k. (Row k -> Row k) -> Row k -> Row k
type RowApply f a = f a
infixr 0 type RowApply as +

type EADT t = Mu (VF.VariantF t)

injEADT 
  ∷ forall f s a b
  . Row.Cons s f a b 
  => IsSymbol s 
  => Functor f 
  => Proxy s 
  -> Algebra f (EADT b)
injEADT label = roll <<< VF.inj label

prjEADT 
  :: forall f s a b
  . Row.Cons s f a b 
  => IsSymbol s
  => Functor f
  => Proxy s 
  -> CoalgebraM Maybe f (EADT b)
prjEADT label = VF.prj label <<< unroll

_VariantF
  :: forall l f v a
   . IsSymbol l
  => Functor f
  => Row.Cons l f _ v
  => Proxy l
  -> L.Prism' (VF.VariantF v a) (f a)
_VariantF label = L.prism' (VF.inj label) (VF.prj label)

_EADT 
  :: forall l f v
  . Row.Cons l f _ v 
  => IsSymbol l 
  => Functor f 
  => Proxy l 
  -> L.Prism' (EADT v) (f (EADT v))
_EADT label = L.prism' (injEADT label) (prjEADT label) 

_Mu ∷ forall f g. L.Iso (f (Mu f)) (g (Mu g)) (Mu f) (Mu g)
_Mu = L.iso roll unroll

attributeAlgebra :: forall f a. Functor f => Algebra f a -> Algebra f (Cofree f a)
attributeAlgebra alg a = alg (map head a) :< a

synthCata :: forall f t a. Recursive t f => Algebra f a -> t -> Cofree f a
synthCata alg = cata (attributeAlgebra alg)