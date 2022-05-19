module Control.Applicative.Phases where

import Prelude

import Control.Apply (lift2)

newtype Phases f a = Phases (forall r. (f a -> r) -> (forall x. f (x -> a) -> Phases f x -> r) -> r)

phases :: forall f a r. (f a -> r) -> (forall x. f (x -> a) -> Phases f x -> r) -> Phases f a -> r
phases lift apply (Phases pa) = pa lift apply

now :: forall f a. f a -> Phases f a
now ma = Phases \_lift _ -> _lift ma

push :: forall f a b. f (a -> b) -> Phases f a -> Phases f b
push mf pa = Phases \_ _apply -> _apply mf pa

delay :: forall f a. Applicative f => Phases f a -> Phases f a
delay ta = pure identity `push` ta

later :: forall f a. Applicative f => f a -> Phases f a
later = delay <<< now

infixl 4 applyFlip as <**>

applyFlip :: forall f a b. Apply f => f a -> f (a -> b) -> f b
applyFlip = lift2 \a f -> f a

runPhasesForwards :: forall f a. Applicative f => Phases f a -> f a
runPhasesForwards = phases identity \mf px -> mf <*> runPhasesForwards px

runPhasesBackwards :: forall f a. Applicative f => Phases f a -> f a
runPhasesBackwards = phases identity \mf px -> runPhasesBackwards px <**> mf

instance Functor f => Functor (Phases f) where
  map f = phases
    do \x -> now (map f x)
    do \y -> push (map (\g x -> f (g x)) y)

instance Applicative f => Apply (Phases f) where
  apply :: forall a b. Phases f (a -> b) -> Phases f a -> Phases f b
  apply = phases meld with
    where
    meld :: Applicative f => f (a -> b) -> Phases f a -> Phases f b
    meld mf = phases (now <<< (mf <*> _)) (push <<< lift2 (<<<) mf)

    with :: forall x. Applicative f => f (x -> a -> b) -> Phases f x -> Phases f a -> Phases f b
    with mg px = phases
      do \ma -> push (lift2 flip mg ma) px
      do \mh -> push (lift2 (\a b k -> k a b) mg mh) <<< lift2 (\x y g h -> g x (h y)) px

instance Applicative f => Applicative (Phases f) where
  pure = now <<< pure