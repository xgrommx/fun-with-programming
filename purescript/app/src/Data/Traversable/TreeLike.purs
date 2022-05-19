module Data.Traversable.TreeLike where

import Prelude

import Control.Applicative.Phases (Phases, delay, now, later, runPhasesForwards, runPhasesBackwards)
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree as Cofree
import Data.Const (Const(..))
import Data.List.Lazy as LList
import Data.Monoid.TreeDiagram (showTreeDiagram, singleton, subtree)
import Data.Newtype as N
import Data.String.CodeUnits as CU
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Effect.Console (log)
import Safe.Coerce (coerce)

showTree :: forall tree a. TreeLike tree => Show a => tree a -> LList.List Char -> LList.List Char
showTree = showTreeDiagram <<< treeFoldMap singleton subtree

printTree :: forall tree a. TreeLike tree => Show a => tree a -> Effect Unit
printTree = log <<< CU.fromCharArray <<< LList.toUnfoldable <<< (_ $ LList.nil) <<< showTree

class Functor tree <= TreeLike tree where
  treeTraverse :: forall f a b. Applicative f => (a -> f b) -> (forall subtree. TreeLike subtree => subtree a -> f (subtree b)) -> tree a -> f (tree b)

instance Traversable f => TreeLike (Cofree f) where
  treeTraverse f g fa = Cofree.mkCofree <$> f (Cofree.head fa) <*> traverse g (Cofree.tail fa)

inorder :: forall f a b tree. Applicative f => TreeLike tree => (a -> f b) -> tree a -> f (tree b)
inorder f t = treeTraverse f (inorder f) t

preorder :: forall f a b tree. Applicative f => TreeLike tree => (a -> f b) -> tree a -> f (tree b)
preorder f = runPhasesForwards <<< treeTraverse (now <<< f) (later <<< preorder f)

postorder :: forall f a b tree. Applicative f => TreeLike tree => (a -> f b) -> tree a -> f (tree b)
postorder f = runPhasesBackwards <<< treeTraverse (now <<< f) (later <<< postorder f)

levelorder :: forall f a b tree. Applicative f => TreeLike tree => (a -> f b) -> tree a -> f (tree b)
levelorder f = runPhasesForwards <<< schedule f
  where
  schedule :: forall subtree. Applicative f => TreeLike subtree => (a -> f b) -> subtree a -> Phases f (subtree b)
  schedule f = treeTraverse (now <<< f) (delay <<< schedule f)

rlevelorder :: forall f a b tree. Applicative f => TreeLike tree => (a -> f b) -> tree a -> f (tree b)
rlevelorder f = runPhasesBackwards <<< schedule f
  where
  schedule :: forall subtree. Applicative f => TreeLike subtree => (a -> f b) -> subtree a -> Phases f (subtree b)
  schedule f = treeTraverse (now <<< f) (delay <<< schedule f)

treeFoldMap :: forall tree m a. Monoid m => TreeLike tree => (a -> m) -> (m -> m) -> tree a -> m
treeFoldMap f g = N.un Const <<< treeTraverse (coerce <<< f) (coerce <<< g <<< treeFoldMap f g)