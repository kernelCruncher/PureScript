module Data.GameItem where

import Prelude

import Data.Maybe (Maybe(..))
--We have two items here. 
data GameItem = Candle | Matches

-- Show does not have built-in compiler support and, as this is an ADT, we can't use newtype derive, either.
instance showGameItem :: Show GameItem where
  show Candle         = "Candle"
  show Matches        = "Matches"

derive instance eqGameItem :: Eq GameItem
derive instance ordGameItem :: Ord GameItem

readItem :: String -> Maybe GameItem
readItem "Candle" = Just Candle
readItem "Matches" = Just Matches
readItem _ = Nothing
