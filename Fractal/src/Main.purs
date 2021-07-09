module Main where

import Prelude
import Fractal (Letter(..), State, initial, initialState, lsystem, productions)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setShadowOffsetY, setFillStyle, setStrokeStyle, strokePath, clearRect, getCanvasHeight, getCanvasWidth, closePath, setShadowOffsetX, setShadowColor, setShadowBlur)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Effect.Ref as Ref
import Effect.Console (logShow)


main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas

  clickCount <- Ref.new 0

  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc

  setStrokeStyle ctx "#000"
  setFillStyle ctx "#F00" --blue

  let --This allows you to define a method in another method.
    interpret :: State -> Letter -> Effect State --This takes the Current state (i.e. x and y coordinates on the Canvas) and makes the changes to the canvas.
    interpret state L = pure $ state { theta = state.theta - Math.tau / 6.0 } --Need Pure as we are returning a Monad.
    interpret state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpret state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx x y -- This draws a line from the old x-y to new x-y.
      pure { x, y, theta: state.theta } --This returns the new State.

  clickListener <- eventListener $ \_ -> do
    logShow "Mouse clicked!"
    count <- Ref.modify (\count -> mod (count + 1) 6 ) clickCount
    clearRect ctx {x :0.0, y: 0.0,  width: width, height: height}
    fillPath ctx $ strokePath ctx $ do
    --moveTo ctx state.x state.y -- moveTo ctx state.x state.y --This moves the canvas icon to the original x and y.
      moveTo ctx initialState.x initialState.y
      _ <- lsystem initial productions interpret count initialState
      closePath ctx
    setShadowOffsetX ctx 100.0
    setShadowOffsetY ctx 50.0
    setShadowBlur ctx 25.0
    setShadowColor ctx "#fa0"
    
  addEventListener (EventType "click") clickListener true (toEventTarget node)

--To Do:  Understand why Javascript crashes.