module Main where

import Prelude

import Combinators (baseTariff, mkRoute, (<:>))
import Data.Array as Array
import Data.Graph (Graph, fromAdjacencyList, shortestPath)
import Data.Int (toNumber)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Types (AdjacencyList, Station(..), printStation)

line50 :: AdjacencyList
line50 = mkRoute $
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
  , Gein
  ]

line51 :: AdjacencyList
line51 = mkRoute $
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
line52 = mkRoute $
  [ Zuid
  , Europaplein
  , DePijp
  , Vijzelgracht
  , Rokin
  , CentraalStation
  , Nooderpark
  , Noord
  ]

line53 :: AdjacencyList
line53 = mkRoute $
  [ CentraalStation
  , Nieuwmarkt
  , Waterlooplein
  , Weesperplein
  , Wibautstraat
  , Amstel
  , Spaklerweg
  , VanDerMadeweg
  , Venserpolder
  , DiemenZuid
  , VerrijnStuartweg
  , Ganzenhoef
  , Kraaiennest
  , Gaasperplas
  ]

line54 :: AdjacencyList
line54 = mkRoute $
  [ CentraalStation
  , Nieuwmarkt
  , Waterlooplein
  , Weesperplein
  , Wibautstraat
  , Amstel
  , Spaklerweg
  , VanDerMadeweg
  , Duivendrecht
  , Strandvliet
  , BiljmerArena
  , Bullewijk
  , Holendrecht
  , Reigersbos
  , Gein
  ]

allRoutes :: AdjacencyList
allRoutes = line50 <:> line51 <:> line52 <:> line53 <:> line54

amsMetroLines :: Graph Station Number
amsMetroLines = fromAdjacencyList allRoutes

withMetroNumber :: List Station -> List { station :: Station, metro :: Array Int }
withMetroNumber xs = do
  x <- xs
  -- TODO: make this algorithm better
  let is50 = 50 <$ List.findIndex (\(Tuple st _) -> st == x) line50
  let is51 = 51 <$ List.findIndex (\(Tuple st _) -> st == x) line51
  let is52 = 52 <$ List.findIndex (\(Tuple st _) -> st == x) line52
  let is53 = 53 <$ List.findIndex (\(Tuple st _) -> st == x) line53
  let is54 = 54 <$ List.findIndex (\(Tuple st _) -> st == x) line54
  pure $ {
    station: x,
    metro: Array.foldl
      (\acc curr -> maybe acc (Array.snoc acc) curr)
      []
      [is50, is51, is52, is53, is54]
  }

prettyPrintPath :: List { station :: Station, metro :: Array Int } -> String
prettyPrintPath xs = withMsg $ List.foldl go "" xs where
  go acc { station, metro } =
    acc <> "\t- " <> (printStation station) <> " with Metro number " <> (show metro) <> " \n"
  withMsg s =
    "Your shortest route would be:\n" <> s <>
    "And it will cost you €" <> show (baseTariff * (toNumber $ List.length xs))

main :: Effect Unit
main = do
  log $ case shortestPath Amstelveenseweg DiemenZuid amsMetroLines of
    Nothing -> "Can't find it!"
    Just path -> prettyPrintPath $ withMetroNumber $ path
