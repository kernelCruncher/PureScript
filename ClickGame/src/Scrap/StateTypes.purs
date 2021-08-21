--Here we create the initial state of the game where each tile has a partner associated with it and a colour. We want to loop through each colour and choose 2 random numbers.
module StateTypes where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


data Colour =  Blue | Green | Red | Yellow

--The derive instance is just for debugging purposes.
derive instance genericColour :: Generic Colour _

instance showColour:: Show Colour where
    show = genericShow

type PanelState = { colour :: Colour, numb1 :: Int, numb2 :: Int}
type GameState = Array PanelState
type CurrentState = {open :: Boolean, partner :: Int }
