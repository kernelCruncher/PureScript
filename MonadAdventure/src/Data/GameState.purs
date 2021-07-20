module Data.GameState where

import Prelude

import Data.Coords (Coords(..), coords)
import Data.GameItem (GameItem(..))
import Data.Map as M
import Data.Set as S
import Data.Tuple (Tuple(..))

--Map associates the Coords with the Game Item.
--The GameState type uses two new data structures: Map and Set, which represent sorted maps and sorted sets respectively. 
--The items property is a mapping from coordinates of the game grid to sets of game items at that location. 
--The player property stores the current coordinates of the player, and the inventory property stores a set of game items currently held by the player.
--The Map and Set data structures are sorted by their keys, can be used with any key type in the Ord type class. 
--This means that the keys in our data structures should be totally ordered. Map k v represents maps from keys of type k to values of type v, so is basically a Dictionary.
newtype GameState = GameState
  { items       :: M.Map Coords (S.Set GameItem) -- You can have multiple items at a set of coordinates.
  , player      :: Coords
  , inventory   :: S.Set GameItem
  }

instance showGameState :: Show GameState where
  show (GameState o) =
    "GameState " <>
    "{ items: "     <> show o.items <>
    ", player: "    <> show o.player <>
    ", inventory: " <> show o.inventory <>
    " }"
-- We create the Initial State. We set the Inventory to empty and the Coords to (0,0).
-- fromFoldable adds the Colelction of Key/value pairs into a Map (i.e. a dictionary)
initialGameState :: GameState
initialGameState = GameState
  { items      : M.fromFoldable [ Tuple (coords 0 1) (S.singleton Candle)
                                , Tuple (coords 0 0) (S.singleton Matches)
                                ]
  , player     : Coords { x: 0, y: 0 }
  , inventory  : S.empty
  }
