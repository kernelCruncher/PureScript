module Colour where

import Prelude
import Effect (Effect)
import Effect.Random (random, randomInt)
import Data.Array ((..), index, length)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Context2D, strokePath, fillPath, arc, setFillStyle)
import Math as Math
import Effect.Console (logShow)

data Colour =  Blue | Green | Red | White | Yellow

createColour :: Colour -> String
createColour Blue = "#00f"
createColour Green = "#080"
createColour Red = "#F00"
createColour White = "#fff"
createColour Yellow = "#fa0"

randomColour :: Effect String
randomColour = do
    let colours :: Array Colour
        colours = [Blue, Green, Red, White, Yellow]
    x <- randomInt 0 (length colours - 1) 
    let colour = index colours x
    case colour of
    --I could have used unsafeIndex (this would have required a instance of the Partial class to be applied) 
    --or fromJust and unsafePartial since I know there is always a corresponding element for the index.
      Just one ->  pure $ createColour one
      Nothing  ->  pure $ createColour Blue
    

circleMaker :: Context2D -> Effect Unit
circleMaker ctx = do
    logShow "Mouse clicked!"
    for_ (1 .. 100) \_ -> do
        x <- random
        y <- random
        r <- random     
        
        let path = arc ctx { x     : x * 600.0
        , y     : y * 600.0
        , radius: r * 50.0
        , start : 0.0
        , end   : Math.tau
        }
        colour <- randomColour
        setFillStyle ctx colour
        fillPath ctx path
        strokePath ctx path