module Game where

import Prelude

import Control.Monad.Except
import Control.Monad.RWS (RWS)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Writer (tell)
import Data.Coords (Coords(..), prettyPrintCoords, coords)
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment(..))
import Data.GameItem (GameItem(..), readItem)
import Data.GameState (GameState(..))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S

type Log = L.List String
--GameEnvironment is the Global Configuration, Log is the writer action, GameState is the State that can be updated. This generates a monad, i.e. m a where a is to be specified later.
type Game = ExceptT String (RWS GameEnvironment Log GameState) -- The code has been refactored to use the ExceptT monad transformer to handle the error messages, and RWS to handle informational messages.

describeRoom :: Game Unit
describeRoom = do
  GameState state <- get -- gets the State. Ask gets the configuration.
  case state.player of
    Coords { x: 0, y: 0 } ->  tell (L.singleton "You are in a dark forest. You see a path to the north.")
    Coords { x: 0, y: 1 } ->  tell (L.singleton "You are in a clearing.")
    _ -> tell (L.singleton "You are deep in the forest.")

--This adds a game item to the player's inventory if it appears in the current room. 
pickUp :: GameItem -> Game Unit
pickUp item = do
  GameState state <- get
  case state.player `M.lookup` state.items of
    Just items
      | item `S.member` items -> do -- the guard | gives an additional qualifier.
          let newItems = M.update (Just <<< S.delete item) state.player state.items
              newInventory = S.insert item state.inventory
          put $ GameState state { items     = newItems -- we can use put to update the game state, and tell to add a message to the log:
                                , inventory = newInventory -- The argument to put uses a record update to modify the game state's items and inventory fields. We use the update function from Data.Map which modifies a value at a particular key. In this case, we modify the set of items at the player's current location, using the delete function to remove the specified item from the set. inventory is also updated, using insert to add the new item to the player's inventory set.
                                }
          lift $ tell (L.singleton ("You now have the " <> show item))
    _ -> throwError "I don't see that item here."

move :: Int -> Int -> Game Unit
move dx dy = modify_ (\(GameState state) -> GameState (state { player = updateCoords state.player }))
  where
  updateCoords :: Coords -> Coords
  updateCoords (Coords p) = coords (p.x + dx) (p.y + dy)

--This action tests whether the player's inventory contains a particular game item.
--This function uses the get action defined in the MonadState type class to read the current game state, and then uses the member function defined in Data.Set to test whether the specified GameItem appears in the Set of inventory items.
has :: GameItem -> Game Boolean
has item = do
  GameState state <- get
  pure $ item `S.member` state.inventory

use :: GameItem -> Game Unit
use Candle = throwError "I don't know what you want me to do with that."
use Matches = do
  hasCandle <- has Candle
  if hasCandle
    then do
      GameEnvironment env <- ask
      lift $ tell (L.fromFoldable [ "You light the candle."
                           , "Congratulations, " <> env.playerName <> "!"
                           , "You win!"
                           ])
    else throwError "You don't have anything to light."

-- This was an exercise. Cheat should put all the items into the player's inventory. Could have used: let inven = foldl (\accum x -> S.union accum x) S.empty values

cheat :: Game Unit
cheat = do
  GameState state <- get
  let values = M.values state.items
  put $ GameState state {items     =  M.empty
                      , inventory =   S.unions values
                      }
                  
game :: Array String -> Game Unit
game ["look"] = do
  GameState state <- get
  lift $ tell (L.singleton ("You are at " <> prettyPrintCoords state.player))
  describeRoom
  for_ (M.lookup state.player state.items) $ \items ->
    lift $ tell (map (\item -> "You can see the " <> show item <> ".") (S.toUnfoldable items :: L.List GameItem))
game ["inventory"] = do
  GameState state <- get
  lift $ tell (map (\item -> "You have the " <> show item <> ".") (S.toUnfoldable state.inventory :: L.List GameItem))
game ["north"] = move 0    1
game ["south"] = move 0    (-1)
game ["west"]  = move (-1) 0
game ["east"]  = move 1    0
game ["take", item] =
  case readItem item of
    Nothing -> throwError "I don't know what item you are referring to."
    Just gameItem -> pickUp gameItem
game ["use", item] =
  case readItem item of
    Nothing -> throwError "I don't know what item you are referring to."
    Just gameItem -> do
      hasItem <- has gameItem
      if hasItem
        then use gameItem
        else throwError"You don't have that item."
game ["debug"] = do
  GameEnvironment env <- ask
  if env.debugMode
    then do
      state :: GameState <- get
      tell (L.singleton (show state))
    else throwError "Not running in debug mode."
game ["cheat"] = do
  GameEnvironment env <- ask
  if env.cheatMode
    then do
      cheat
      lift $ tell (L.singleton "All items added to inventory")
    else throwError "Not running in cheat mode."
game [] = pure unit
game _  = throwError"I don't understand."
