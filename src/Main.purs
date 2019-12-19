module Main where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Graph (Graph, fromAdjacencyList, shortestPath)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Types (Station(..))

type Adjacency = Tuple Station (List (Tuple Station Int))
type AdjacencyList = List Adjacency

baseTariff :: Int
baseTariff = 10

mkLines :: Int -> Array Station -> AdjacencyList
mkLines num arr = edges.acc where
  edges = Array.foldl toEdge { i: 0, acc: Nil } arr
  toEdge { i, acc } a = {
    i: i + 1,
    acc: (a `has` (adjacentArray i arr)) : acc
  }

has :: Station -> Array Station -> Adjacency
has station siblings = Tuple station (mkSiblings siblings) where
  mkSiblings :: Array Station -> List (Tuple Station Int)
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

line50 :: AdjacencyList
line50 = mkLines 50 $
  [ Isolatorweg
  , DeVlugtlaan
  , JanVanGalenstraat
  , Postjesweg
  , Lelylan
  , Heemstedestraat
  , HenkSneevlietweg
  , Amstelveenseweg
  , Zuid
  , Rai
  , Overamstel
  , VanDerMadeweg
  , Duivendrecht
  , Strandvliet
  , BiljmerArena
  , Bullewijk
  , Holendrecht
  , Reigersbos
  ]

line51 :: AdjacencyList
line51 = mkLines 51 $
  [ Isolatorweg
  , DeVlugtlaan
  , JanVanGalenstraat
  , Postjesweg
  , Lelylan
  , Heemstedestraat
  , HenkSneevlietweg
  , Amstelveenseweg
  , Zuid
  , Rai
  , Overamstel
  , Spaklerweg
  , Amstel
  , Wibautstraat
  , Weesperplein
  , Waterlooplein
  , Nieuwmarkt
  , CentraalStation
  ]

line52 :: AdjacencyList
line52 = mkLines 52 $
  [ Zuid
  , Europaplein
  , DePijp
  , Vijzelgracht
  , Rokin
  , CentraalStation
  , Nooderpark
  , Noord
  ]

amsMetroLines :: Graph Station Int
amsMetroLines = fromAdjacencyList $ line50 `unionEdges` line51 `unionEdges` line52

withMetroNumber :: List Station -> List { station :: Station, metro :: Array Int }
withMetroNumber xs = do
  x <- xs
  let is50 = 50 <$ List.findIndex (\(Tuple st _) -> st == x) line50
  let is51 = 51 <$ List.findIndex (\(Tuple st _) -> st == x) line51
  let is52 = 52 <$ List.findIndex (\(Tuple st _) -> st == x) line52
  pure { station: x, metro: Array.foldl (\acc curr -> maybe acc Array.singleton curr) [] [is50, is51, is52] }

main :: Effect Unit
main = do
  logShow $ case shortestPath Amstelveenseweg Nieuwmarkt amsMetroLines of
    Nothing -> "Can't find it!"
    Just path -> show $ withMetroNumber $ path
