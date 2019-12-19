module Combinators
  ( baseTariff
  , mkRoute
  , unionEdges
  , (<:>)
  ) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Types (Adjacency, Station, AdjacencyList)

baseTariff :: Number
baseTariff = 0.1

mkRoute :: Array Station -> AdjacencyList
mkRoute arr = edges.acc where
  edges = Array.foldl toEdge { i: 0, acc: Nil } arr
  toEdge { i, acc } a = {
    i: i + 1,
    acc: (a `has` (adjacentArray i arr)) : acc
  }

has :: Station -> Array Station -> Adjacency
has station siblings = Tuple station (mkSiblings siblings) where
  mkSiblings :: Array Station -> List (Tuple Station Number)
  mkSiblings = List.fromFoldable <<< map (\st -> Tuple st baseTariff)

-- | Gets the previous and the next item of the current one and merget them
-- | together. If no item found, defaults to an empty array
-- |
-- | ```purs
-- | adjacentArray 0 [7, 8] == [8]
-- | adjacentArray 1 [7, 8, 9] == [7, 9]
-- | adjacentArray 2 [7, 8, 9] == [8]
-- | adjacentArray -1 [7, 8, 9] == []
-- | ```
adjacentArray :: ∀ a. Int -> Array a -> Array a
adjacentArray i arr = (def $ arr !! (i-1)) <> (def $ arr !! (i+1)) where
  def = maybe [] Array.singleton

unionEdges :: AdjacencyList -> AdjacencyList -> AdjacencyList
unionEdges src xs = List.foldl go src xs where

  go :: AdjacencyList -> Adjacency -> AdjacencyList
  go acc x@(Tuple station _) = do
    case findStopIx station acc of
      Nothing -> List.snoc acc x
      Just ix -> unsafePartial $ fromJust (List.modifyAt ix (mergeInner x) acc)

  findStopIx :: ∀ a. Station -> List (Tuple Station a) -> Maybe Int
  findStopIx st = List.findIndex (\(Tuple s _) -> s == st)

  mergeInner :: Adjacency -> Adjacency -> Adjacency
  mergeInner (Tuple stationX listX) (Tuple stationA listA) = Tuple stationA merged where
    merged = List.unionBy (\(Tuple stX _) (Tuple stA _) -> stX == stA) listA listX

infixl 6 unionEdges as <:>
