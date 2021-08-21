module PlayButton where

import Prelude

import Data.Traversable (for_)
import Effect (Effect)
import Graphics.Canvas (Context2D, fillPath, fillText, rect, setFillStyle)

--The point of 'play' is to turn all the Canvas elements to Grey so that they look clickable. I'll add some text.
-- Question: why did Map not work? i.e. map (\n -> makeGrey n width height) array
play ::  Array (Effect Context2D) -> Number -> Number -> Effect Unit
play array width height = do
      _ <- for_ array $ \x -> do 
                  y <- x 
                  setUp y width height     
      pure unit

setUp :: Context2D -> Number -> Number -> Effect Unit
setUp ctx width height = do
    makeGrey ctx width height
    addText ctx width height
  
makeGrey :: Context2D -> Number -> Number -> Effect Unit
makeGrey ctx width height = do
      setFillStyle ctx "#808080"
      fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: width
            , height: height
            }

addText :: Context2D -> Number -> Number -> Effect Unit
addText ctx width height = do
      setFillStyle ctx "#000000"
      fillText ctx "CLICK ME" (width/2.0 - 30.0) (height/2.0 + 5.0)