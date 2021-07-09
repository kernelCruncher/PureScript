module Fractal where

import Prelude
import Data.Array (concatMap, foldM)

--The lsystem method takes a specification and renders it to the canvas. 
--It takes the initial (i.e. starting sequence of letters) and productions (i.e. what each letter gets mapped to) arguments
-- as well as an interpret function which is what does the rendering (i.e. move left when encountering the letter L and right with R). 
-- The n is #iterations, and the state argument, s, records what state the canvas is now.

lsystem :: forall a m s
         . Monad m
         => Array a
         -> (a -> Array a)
         -> (s -> a -> m s)
         -> Int
         -> s
         -> m s
lsystem init prod interpret n state = go init n
  where
  ----The function foldM is similar to 'foldl', but the result is encapsulated in a monad. So we apply interpret to s and accumulate the changes to the current state, called state.
  go s 0 = foldM interpret state s 
  go s i = go (concatMap prod s) (i - 1) --This is recursive. 

data Letter = L | R | F

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = [F, R, R, F, R, R, F, R, R]

productions :: Letter -> Sentence
productions L = [L]
productions R = [R]
productions F = [F, L, F, R, R, F, L, F]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }
