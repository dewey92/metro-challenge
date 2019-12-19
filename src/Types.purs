module Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.String (joinWith)
import Data.String.Extra (words)
import Data.Tuple (Tuple)

data Station
  = Isolatorweg
  | DeVlugtlaan
  | JanVanGalenstraat
  | Postjesweg
  | Lelylan
  | Heemstedestraat
  | HenkSneevlietweg
  | Amstelveenseweg
  | Zuid
  | Rai
  | Overamstel
  | VanDerMadeweg
  | Duivendrecht
  | Strandvliet
  | BiljmerArena
  | Bullewijk
  | Holendrecht
  | Reigersbos
  | Gein
  | Noord
  | Nooderpark
  | CentraalStation
  | Rokin
  | Vijzelgracht
  | DePijp
  | Europaplein
  | Nieuwmarkt
  | Waterlooplein
  | Weesperplein
  | Wibautstraat
  | Amstel
  | Spaklerweg
  | Venserpolder
  | DiemenZuid
  | VerrijnStuartweg
  | Ganzenhoef
  | Kraaiennest
  | Gaasperplas

derive instance genericStation :: Generic Station _
derive instance eqStation :: Eq Station
derive instance ordStation :: Ord Station

instance showStation :: Show Station where
  show = genericShow

type Adjacency = Tuple Station (List (Tuple Station Number))
type AdjacencyList = List Adjacency

printStation :: Station -> String
printStation = show >>> words >>> joinWith " "
