module Data.Coords where

import Prelude

newtype Coords = Coords
  { x :: Int
  , y :: Int
  }

--This is what will allow us to print our coordinates to the console for the user to see. We create an instance of the the show class for coordinates. 
-- Show is not a class that we can derive from the Compiler. However, we could use "newtype derive" which defers to the underlying type (a record here), but this would ignore the constructor in the Show, i.e. no coords.
instance showCoords :: Show Coords where
  show (Coords p) = "Coords " <>
                    "{ x: " <> show p.x <>
                    ", y: " <> show p.y <>
                    " }"
-- Here we derive instances for the Equal and Ordering of the Coordinate class.
--The Eq and Ord classes have special built-in compiler support and their instances can be derived from all types.
derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords
-- derive newtype instance showCoords :: Show Coords

coords :: Int -> Int -> Coords
coords x y = Coords { x: x, y: y }

prettyPrintCoords :: Coords -> String
prettyPrintCoords (Coords p) = "(" <> show p.x <> ", " <> show p.y <> ")"
