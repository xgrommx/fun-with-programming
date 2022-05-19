module Data.Monoid.TreeDiagram where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.List.Lazy as LList
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicate, unfoldr)
import Effect (Effect)
import Effect.Console (log)

type GraphEnvironment =
  { isLeftmost :: Boolean -- ^ whether this part of the graph is the leftmost part of the graph
  , isRightmost :: Boolean -- ^ whether this part of the graph is the rightmost part of the graph
  , uptickIndex :: Int -- ^ index  of the uptick, relative to this part of the graph
  }

data TreeDiagram
  = Empty
  | NonEmpty
      { graph :: GraphEnvironment -> LList.List Char -> LList.List Char -- ^ the top line of the diagram
      , graphWidth :: Int -- ^ number of characters in the graph
      , graphIndent :: Int -- ^ left whitespace needed to align graph with rows underneath
      , graphDedent :: Int -- ^ right whitespace needed to pad graph to maximum row width
      , rows :: LList.List { x :: Int, y :: LList.List Char -> LList.List Char } -- ^ width and definition of each row under the graph line
      , leftLimit :: { x :: Int, y :: Int } -- ^ index of lines without any left whitespace
      , rightLimit :: { x :: Int, y :: Int } -- ^ index of lines of maximum row width
      }

instance Semigroup TreeDiagram where
  append Empty d = d
  append d Empty = d
  append x@(NonEmpty a) y@(NonEmpty b) = NonEmpty
    { graph: \o ->
        let
          uptickIndex' = o.uptickIndex - a.graphWidth
          midline =
            if 0 <= uptickIndex' && uptickIndex' < graphPadding then replicateChar uptickIndex' '─'
              <<< LList.cons '┴'
              <<<
                replicateChar (graphPadding - 1 - uptickIndex') '─'
            else replicateChar graphPadding '─'
        in
          a.graph o { isRightmost = false }
            <<< midline
            <<<
              b.graph o { isLeftmost = false, uptickIndex = uptickIndex' - graphPadding }
    , graphWidth: a.graphWidth + graphPadding + b.graphWidth
    , graphIndent: a.graphIndent
    , graphDedent: b.graphDedent
    , rows: alongside (width x + padding) a.rows b.rows
    , leftLimit: a.leftLimit
    , rightLimit: b.rightLimit
    }
    where
      graphPadding = a.graphDedent + padding + b.graphIndent
      padding = fromEnum (r2.x <= r1.y && r1.x <= r2.y)
      r1 = a.rightLimit
      r2 = b.leftLimit

instance Monoid TreeDiagram where
  mempty = Empty

concatShowS :: LList.List (LList.List Char -> LList.List Char) -> LList.List Char -> LList.List Char
concatShowS = foldr (<<<) identity

head :: String -> Maybe Char
head = CU.charAt 0

tail :: String -> Maybe String
tail s = _.tail <$> CU.uncons s

fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap CU.singleton

toChars :: forall f. Unfoldable f => String -> f Char
toChars = unfoldr \str -> Tuple <$> head str <*> tail str

replicateChar :: Int -> Char -> LList.List Char -> LList.List Char
replicateChar n = concatShowS <<< replicate n <<< LList.cons

prependToAll' :: forall a. a -> LList.List a -> LList.List a 
prependToAll' sep l = tailRec go {acc: identity, list: l} LList.nil
  where
    go {acc, list} = case LList.uncons list of
      Nothing -> Done acc
      Just r -> Loop { acc : acc <<< (\x ->  sep LList.: r.head LList.: x), list: r.tail }

intersperse :: forall a. a -> LList.List a -> LList.List a
intersperse sep l = case LList.uncons l of
  Nothing -> LList.nil
  Just r -> r.head LList.: prependToAll' sep r.tail

showTreeDiagram :: TreeDiagram -> LList.List Char -> LList.List Char
showTreeDiagram Empty = identity
showTreeDiagram (NonEmpty r) =
  let
    graphLine =
      replicateChar r.graphIndent ' ' <<<
        r.graph
          { isLeftmost: true
          , isRightmost: true
          , uptickIndex: r.graphWidth -- don't show the uptick
          }
    rowLines = _.y <$> r.rows
  in
    concatShowS <<< intersperse (LList.cons '\n') $ graphLine LList.: rowLines

printTreeDiagram :: TreeDiagram -> Effect Unit
printTreeDiagram = log <<< fromChars <<< (_ $ LList.nil) <<< showTreeDiagram

singleton :: forall a. Show a => a -> TreeDiagram
singleton a = NonEmpty
  { graph: const $ (\x l -> toChars (show x) <> l) a
  , graphWidth: CU.length $ show a
  , graphIndent: 0
  , graphDedent: 0
  , rows: LList.nil
  , leftLimit: { x: 0, y: 0 }
  , rightLimit: { x: 0, y: 0 }
  }

width :: TreeDiagram -> Int
width Empty = 0
width (NonEmpty d) = d.graphIndent + d.graphWidth + d.graphDedent

height :: TreeDiagram -> Int
height Empty = 0
height (NonEmpty d) = 1 + LList.length d.rows

alongside :: Int -> LList.List { x :: Int, y :: LList.List Char -> LList.List Char } -> LList.List { x :: Int, y :: LList.List Char -> LList.List Char } -> LList.List { x :: Int, y :: LList.List Char -> LList.List Char }
alongside n xs ys = case LList.uncons xs, LList.uncons ys of
  Just r1, Just r2 -> {x: n + r2.head.x, y: r1.head.y <<< replicateChar (n - r1.head.x) ' ' <<< r2.head.y} LList.: alongside n r1.tail r2.tail
  _, Nothing -> xs
  Nothing, _ -> (\({x: my, y: dy}) -> {x: n + my, y: replicateChar n ' ' <<< dy}) <$> ys

downtick :: GraphEnvironment -> LList.List Char -> LList.List Char
downtick r = case r.isLeftmost, r.isRightmost, r.uptickIndex == 0 of
  false, false, false -> LList.cons '┬'
  false, false, true -> LList.cons '┼'
  false, true, false -> LList.cons '┐'
  false, true, true -> LList.cons '┤'
  true, false, false -> LList.cons '┌'
  true, false, true -> LList.cons '├'
  true, true, false -> LList.cons '╷'
  true, true, true -> LList.cons '│'

subtree :: TreeDiagram -> TreeDiagram
subtree Empty = NonEmpty
  { graph: downtick
  , graphWidth: 1
  , graphIndent: 0
  , graphDedent: 0
  , rows: LList.fromFoldable [ { x: 1, y: LList.cons '│' }, { x: 1, y: LList.cons '╵' } ]
  , leftLimit: { x: 1, y: 2 }
  , rightLimit: { x: 1, y: 2 }
  }
subtree (NonEmpty r) = NonEmpty
  { graph: downtick
  , graphWidth: 1
  , graphIndent: uptickIndent
  , graphDedent: r.graphIndent + r.graphWidth + r.graphDedent - 1 - uptickIndent
  , rows:
      { x: uptickIndent + 1, y: replicateChar uptickIndent ' ' <<< LList.cons '│' }
        LList.: { x: r.graphIndent + r.graphWidth, y: replicateChar r.graphIndent ' ' <<< graphLine }
        LList.: r.rows
  , leftLimit: { x: if r1.x > 1 then r1.x + 2 else if r.graphWidth > 1 then 2 else 1, y: r1.y + 2 }
  , rightLimit: { x: if r2.x > 1 then r2.x + 2 else if r.graphWidth > 2 then 2 else 1, y: r2.y + 2 }
  }
  where
  uptickIndent = r.graphIndent + uptickIndex
  uptickIndex = r.graphWidth `div` 2
  r1 = r.leftLimit
  r2 = r.rightLimit
  graphLine = r.graph
    { isLeftmost: true
    , isRightmost: true
    , uptickIndex: uptickIndex
    }