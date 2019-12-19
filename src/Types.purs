module Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (joinWith)
import Data.String.Extra (words)

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
  | Gaasperplaas

derive instance genericStation :: Generic Station _
derive instance eqStation :: Eq Station
derive instance ordStation :: Ord Station

instance showStation :: Show Station where
  show = genericShow

printStation :: Station -> String
printStation = show >>> words >>> joinWith " "

-- | `Stop` indicates the idea of a station having multiple metros go through it
newtype Stop = Stop { station :: Station, metro :: Array Int }

derive instance eqStop :: Eq Stop

instance ordStop :: Ord Stop where
  compare (Stop a) (Stop b) = compare a.station b.station

instance showStop :: Show Stop where
  show (Stop { station, metro }) = "Stop={" <> show station <> ", " <> show metro <> "}"
