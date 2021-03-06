module Data.GameEnvironment where

type PlayerName = String
--Observe that a newtype can have multiple arguments in a record format.
newtype GameEnvironment = GameEnvironment
  { playerName    :: PlayerName
  , debugMode     :: Boolean
  , cheatMode     :: Boolean
  }

gameEnvironment :: PlayerName -> Boolean -> Boolean -> GameEnvironment
gameEnvironment playerName debugMode cheatMode = GameEnvironment
  { playerName    : playerName
  , debugMode     : debugMode
  , cheatMode     : cheatMode
  }
