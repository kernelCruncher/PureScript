module Main where

import Prelude
import Colour (circleMaker)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (setStrokeStyle, getContext2D, getCanvasElementById)
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.DOM.Document (toParentNode)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.HTML (window)
import Web.DOM.Element (toEventTarget)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc

  setStrokeStyle ctx "#000"

  clickListener <- eventListener $ \_ -> do
        circleMaker ctx
      
  addEventListener (EventType "click") clickListener true (toEventTarget node)