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
import Data.Maybe (Maybe(..), fromJust)
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
has station siblings = Tuple station (mkWeight siblings) where
  mkWeight :: Array Station -> List (Tuple Station Number)
  mkWeight = Array.foldl (\acc st -> (Tuple st baseTariff) : acc) Nil

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
adjacentArray i arr = Array.catMaybes [arr !! (i - 1), arr !! (i + 1)]

unionEdges :: AdjacencyList -> AdjacencyList -> AdjacencyList
unionEdges src xs = List.foldr go src xs where

  go :: Adjacency -> AdjacencyList -> AdjacencyList
  go x@(Tuple station _) acc = do
    case List.findIndex (\(Tuple s _) -> s == station) acc of
      Nothing -> x : acc
      Just ix -> unsafePartial $ fromJust (List.modifyAt ix (mergeInner x) acc)

  mergeInner :: Adjacency -> Adjacency -> Adjacency
  mergeInner (Tuple _ listX) (Tuple stationA listA) = Tuple stationA mergedAdjacency where
    mergedAdjacency = List.union listA listX

infixl 6 unionEdges as <:>
