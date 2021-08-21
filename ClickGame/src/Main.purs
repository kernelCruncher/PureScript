module Main where

import GameEnvironment (Environment, createInitialEnv, Colour (..))
import GameState (initialState)
import Prelude
import Control.Monad.RWS (RWS, tell)
import CanvasComponent (canvasLogic, Game, colourCanvas)
import Control.Monad.RWS (RWSResult(..), runRWS)
import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (logShow)
import Graphics.Canvas (getCanvasElementById, getCanvasHeight, getCanvasWidth)
import Initialisation (getCanvasEltArray, getCanvasElt)
import Partial.Unsafe (unsafePartial)
import PlayButton (play)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, ParentNode)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

canvasCode :: Environment -> ParentNode -> Number -> Number -> Effect Unit
canvasCode env doc width height = do
    let RWSResult _ result __ =  runRWS (canvasHelper doc width height) env initialState
    --game <- canvasHelper doc width height
    --let RWSResult _ result __ = runRWS game env initialState
    result

canvasHelper :: ParentNode -> Number -> Number -> Effect (Game Unit)
canvasHelper doc width height = unsafePartial do
    pure $ for_ (1..8) (\i -> do
                Just canvasNode <- querySelector (QuerySelector ("#canvas" <> show i)) doc             
                canvasListener <- eventListener $ \_ -> do                  
                  --tell ["This is clicked" <> show i]
                  --   let Game directive = updateCanvasState i -- this returns the commands, and I need to feed these into the UpdateCanvasColour method.
                  --  updateCanvasColour directive width height
                  a <- canvasLogic i width height
                  pure a
                addEventListener (EventType "click") canvasListener true (toEventTarget canvasNode)
              )

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas1"
  -- All Canvas elements in this app have same width and height.
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas

  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just playNode <- querySelector (QuerySelector "#play") doc
  let canvasarray = getCanvasEltArray -- need to check whether this will store old ctx or will be auto-updated any time changes are made.

  startListener <- eventListener $ \_ -> do
        play canvasarray width height
        initEnv <- createInitialEnv
        canvasCode initEnv doc width height

  addEventListener (EventType "click") startListener true (toEventTarget playNode)

  --Might need to use another way to add event listeners to fix this, like with Smolder. 
  --The #! operator can be advantageous,which is an alias for withEvent :: a -> EventHandlers e -> a 
  --e.g. button #! onClick (const Increment) $ text "Increment"