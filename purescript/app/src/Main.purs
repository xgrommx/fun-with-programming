module Main where

import Prelude

import Data.Add (Add, AddF(..), add)
import Data.Functor.Variant as VF
import Data.Mul (Mul, MulF(..), _mul, mul)
import Data.Traversable.TreeLike (printTree)
import Data.Val (Val, ValF(..), val)
import EADT (type (+), EADT, synthCata)
import Effect (Effect)
import Effect.Console (log)
import Matryoshka (Algebra, cata)

expr ∷ EADT (Val + Add + Mul + ())
expr = add (mul (add (val 10) (val 0)) (add (val 30) (mul (val 40) (val 0)))) (val 10)

exprShowAlg ∷ forall r. Algebra (VF.VariantF r) String -> Algebra (VF.VariantF (Val + Add + r)) String
exprShowAlg = VF.onMatch
  { val: case _ of ValF x -> show x
  , add: case _ of AddF x y -> "(" <> x <> " + " <> y <> ")" }

exprShowAlg2 ∷ forall r. Algebra (VF.VariantF r) String -> Algebra (VF.VariantF (Val + Add + Mul + r)) String
exprShowAlg2 = exprShowAlg
  >>> VF.on _mul case _ of MulF x y -> "(" <> x <> " * " <> y <> ")"

exprShowAlg' :: Algebra (VF.VariantF (Val + Add + Mul + ())) String
exprShowAlg' = VF.match 
  { val: case _ of ValF x -> show x
  , add: case _ of AddF x y -> "(" <> x <> " + " <> y <> ")"
  , mul: case _ of MulF x y -> "(" <> x <> " * " <> y <> ")" }

main :: Effect Unit
main = do
  -- log $ cata (VF.case_ # exprShowAlg2) expr
  printTree $ synthCata exprShowAlg' expr
  -- log "Hello sailor!"
