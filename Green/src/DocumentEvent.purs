module DocumentEvent
  (addEvent
  , EventHandler
  , EventType
  , clickEvent
  , inputEvent
  ) where

import Prelude

import Effect (Effect)
import Web.Event.Event (Event)

--The type keyword here is odd. Type defines a type - and we have a function which takes an Event and returns an Effect Unit. I assume it's to do with 
--the Foreign Function Interface. We need to define a type for the addEvent method.

type EventHandler = Event -> Effect Unit

--The below creates a new algebraic data type called EventType. That it is :: Type means it is defined only through JavaScript.
-- Usually we have data EventType :: EventType String, but we only want to be able to define it in JS. There is no way to define it in PureScript with the Type keyword.
foreign import data EventType :: Type 

--These are just constants defined in Javascript.
foreign import clickEvent :: EventType

foreign import inputEvent :: EventType

--The addEvent method takes in an EventType argument. In an earlier iteration it was just a string, now we have a specific kind of string.
foreign import addEvent :: EventType -> EventHandler -> Effect Unit