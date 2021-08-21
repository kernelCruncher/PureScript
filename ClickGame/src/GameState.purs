module GameState where

newtype GameState = GameState {open :: Boolean, pairs :: Int }

initialState :: GameState
initialState = GameState { open: false, pairs : 0}