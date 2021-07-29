module Shapes (Shape (..), area, Position) where

import Prelude

data Shape
  = Circle Position Number
  | Square Position Number
  | Rectangle Position Number Number
  | Triangle Position Number Number

type Position = {x :: Number, y :: Number}

area :: Shape → Number
area (Circle _ radius) = 3.14 * radius * radius
area (Square _ side) = side * side
area (Rectangle _ width height) = width * height
area (Triangle _ side1 side2) = (side1 * side2) / 2.0
