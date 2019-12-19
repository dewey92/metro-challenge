module Main where

import Prelude

import Data.Array (foldl, singleton, (!!))
import Data.Graph (AdjacencyList, Graph, fromAdjacencyList, shortestPath)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (joinWith)
import Data.String.Extra (words)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Types (Station(..), Stop(..))

printStation :: Station -> String
printStation = show >>> words >>> joinWith " "

baseTariff :: Int
baseTariff = 10

mkLines :: Int -> Array Station -> AdjacencyList Stop Int
mkLines num arr = edges.acc where
  edges = foldl toEdge { i: 0, acc: Nil } arr
  toEdge { i, acc } a = {
    i: i + 1,
    acc: (has num (Stop { station: a, metro: [num] }) (adjacentArray i arr)) : acc
  }

has :: Int -> Stop -> Array Station -> _
has num stop siblings = Tuple stop (List.fromFoldable $ mkSiblings siblings) where
  mkSiblings :: Array Station -> List (Tuple Stop Int)
  mkSiblings = List.fromFoldable <<< map (\a -> Tuple (Stop { station: a, metro: [num] }) baseTariff)

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
  def = maybe [] singleton

unionEdges :: AdjacencyList Stop Int -> AdjacencyList Stop Int -> AdjacencyList Stop Int
unionEdges src xs = List.foldl go src xs where

  go :: AdjacencyList Stop Int -> Tuple Stop (List (Tuple Stop Int)) -> AdjacencyList Stop Int
  go acc x@(Tuple (Stop { station, metro }) _) = do
    case findStopIx station acc of
      Nothing -> List.snoc acc x
      Just ix -> unsafePartial $ fromJust (List.modifyAt ix (mergeInner x) acc)

  findStopIx :: ∀ a. Station -> List (Tuple Stop a) -> Maybe Int
  findStopIx st = List.findIndex (\(Tuple (Stop { station: s }) _) -> s == st)

  mergeInner :: Tuple Stop (List (Tuple Stop Int)) -> Tuple Stop (List (Tuple Stop Int)) -> Tuple Stop (List (Tuple Stop Int))
  mergeInner (Tuple (Stop { station: stx, metro: mx }) listX) (Tuple (Stop { station: sta, metro: ma }) as) = Tuple (Stop { station: sta, metro: ma <> mx }) merged where
    merged = List.foldl goInner as listX

  goInner :: List (Tuple Stop Int) -> Tuple Stop Int -> List (Tuple Stop Int)
  goInner acc x@(Tuple (Stop { station, metro }) _) = do
    case findStopIx station acc of
      Nothing -> List.snoc acc x
      Just ix -> unsafePartial $ fromJust (List.modifyAt ix (modifyInner metro) acc)

  modifyInner :: Array Int -> Tuple Stop Int -> Tuple Stop Int
  modifyInner accMetro (Tuple (Stop { station, metro }) n) =
    Tuple (Stop { station, metro: accMetro <> metro }) n

line50 :: AdjacencyList Stop Int
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

graph50 :: Graph Stop Int
graph50 = fromAdjacencyList line50

line51 :: AdjacencyList Stop Int
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

graph51 :: Graph Stop Int
graph51 = fromAdjacencyList line51

line52 :: AdjacencyList Stop Int
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

graph52 :: Graph Stop Int
graph52 = fromAdjacencyList line52

main :: Effect Unit
main = do
  logShow $ line51 `unionEdges` line52
  -- logShow $ shortestPath Zuid Overamstel graph50
