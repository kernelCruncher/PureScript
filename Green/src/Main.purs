module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

import DocumentEvent (addEvent, inputEvent)

import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode, HTMLDocument)
import Web.HTML.HTMLInputElement (fromEventTarget, value, HTMLInputElement)

import Web.DOM.Element (Element, toNode)
import Web.DOM.Node (setTextContent)
import Web.DOM.ParentNode (querySelector, QuerySelector(QuerySelector))

import Web.Event.Event (Event, target)
import Web.Event.EventTarget (EventTarget)

rootSelector :: QuerySelector
rootSelector = QuerySelector ("#root")

selectFromDocument :: HTMLDocument -> Effect (Maybe Element)
selectFromDocument doc = querySelector rootSelector (toParentNode doc)

updateText :: String -> Maybe Element -> Effect Unit
updateText str (Just el) = setTextContent str (toNode el)
updateText _ _ = pure unit

inputValue :: Maybe HTMLInputElement -> Effect String
inputValue (Just el) = value el
inputValue _ = pure ""

targetValue :: Maybe EventTarget -> Effect String
targetValue (Just et) = inputValue (fromEventTarget et)
targetValue _ = pure ""

eventValue :: Event -> Effect String
eventValue evt = targetValue (target evt)

inputEventHandler :: Maybe Element -> Event -> Effect Unit
inputEventHandler el evt = do 
  str <- eventValue evt
  updateText str el

main :: Effect Unit
main = do
  w <- window
  d <- document w
  el <- selectFromDocument d
  addEvent inputEvent (inputEventHandler el)

-- module Main where

-- import Prelude
-- import Data.Maybe (Maybe(..))
-- import Effect (Effect)
-- import Effect.Console (log)
-- import Web.HTML (window)
-- import Web.HTML.Window (document)
-- import Web.HTML.HTMLDocument (toParentNode, HTMLDocument)
-- import Web.DOM.Element (Element, toNode)
-- import Web.DOM.Node (textContent)
-- import Web.DOM.ParentNode (querySelector, QuerySelector(QuerySelector))
-- import Web.DOM.Node (textContent, setTextContent)
-- import Web.Event.Event (Event, EventType(..), target)
-- import Web.Event.EventTarget (EventTarget)
-- import Web.HTML.HTMLInputElement (fromEventTarget, value, HTMLInputElement)
-- import DocumentEvent (addEvent, inputEvent)

-- rootSelector :: QuerySelector
-- rootSelector = QuerySelector ("#root")

-- maybeText :: Maybe Element -> Effect String
-- maybeText (Just el) = textContent  (toNode el)
-- maybeText _ = pure ""

-- selectFromDocument :: HTMLDocument -> Effect (Maybe Element)
-- selectFromDocument doc = querySelector rootSelector (toParentNode doc)

-- updateText :: String -> Maybe Element -> Effect Unit
-- updateText str (Just el) = setTextContent str (toNode el)
-- updateText _ _ = pure unit

-- inputEvent :: EventType
-- inputEvent = EventType ("input")

-- inputEventHandler :: Maybe Element -> Event -> Effect Unit
-- inputEventHandler el evt = do
--   str <- eventValue evt
--   updateText str el

-- eventValue :: Event -> Effect String
-- eventValue evt = targetValue (target evt)

-- targetValue :: Maybe EventTarget -> Effect String
-- targetValue (Just et) = inputValue (fromEventTarget et)
-- targetValue _ = pure ""

-- inputValue :: Maybe HTMLInputElement -> Effect String
-- inputValue (Just el) = value el
-- inputValue _ = pure ""

-- -- main :: Effect Unit
-- -- main = window >>= document >>= selectFromDocument >>= maybeText >>= log

-- main :: Effect Unit
-- main = do
--   w <- window
--   d <- document w
--   el <- selectFromDocument d

--   -- str <- maybeText el
--   -- updateText "Hey! It worked!" el

--   -- eh <- eventListener (inputEventHandler el)
--   -- addEventListener inputEvent eh true (toEventTarget d)
--   -- addEvent "input" (inputEventHandler el)
--     addEvent inputEvent (inputEventHandler el)