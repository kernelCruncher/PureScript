module CanvasComponent where

import Prelude

import Control.Monad.RWS (RWS, get, ask, put, tell)
import Effect (Effect)
import Effect.Aff (delay, Milliseconds(..), launchAff_)
import Effect.Exception (error, throwException)
import GameEnvironment (Colour(..), Environment)
import GameState (GameState (..))
import Graphics.Canvas (Context2D, fillPath, rect, setFillStyle)
import Initialisation (getCanvasElt)
import PlayButton (setUp)
import Effect.Console (logShow)
import Data.Tuple


type Log = Array String
type Game = RWS Environment Log GameState
type Directive = {act:: States, id1 :: Tuple Int Colour, id2:: Tuple Int Colour }
data States = Half | Pair | Clear

createColour :: Colour -> String
createColour Blue = "#00f"
createColour Green = "#080"
createColour Red = "#F00"
createColour Yellow = "#fa0"

colourCanvas:: Effect Context2D -> Colour -> Number -> Number -> Effect Unit
colourCanvas effCtx colour width height = do
      ctx <- effCtx
      setFillStyle ctx $ createColour colour
      fillPath ctx $ rect ctx
            { x: 0.0
            , y: 0.0
            , width: width
            , height: height
            }
      logShow "Reset"

findColour :: Environment -> Int -> Colour
findColour [a,b,c,d] canvasInt | a.numb1 == canvasInt || a.numb2 == canvasInt = a.colour 
                            | b.numb1  == canvasInt || b.numb2 == canvasInt = b.colour
                            | c.numb1 == canvasInt ||  c.numb2 == canvasInt = c.colour
                            | d.numb1  == canvasInt || d.numb2 == canvasInt = d.colour 
findColour _ _ = Blue

resetCanvas:: Effect Context2D -> Effect Context2D -> Number-> Number -> Effect Unit
resetCanvas ctx1 ctx2 width height = do
      ctx1Ed <- ctx1
      setUp ctx1Ed width height
      ctx2Ed <- ctx2
      setUp ctx2Ed width height

getPartner :: Environment -> Int -> Int
getPartner [a, b, c, d] canvasInt   | a.numb1 == canvasInt = a.numb2
                                    | a.numb2 == canvasInt = a.numb1
                                    | b.numb1 == canvasInt = b.numb2
                                    | b.numb2 == canvasInt = b.numb1
                                    | c.numb1 == canvasInt = c.numb2
                                    | c.numb2 == canvasInt = c.numb1
                                    | d.numb1 == canvasInt = d.numb2
                                    | d.numb2 == canvasInt = d.numb1
getPartner _ _ = 0

-- --This is the main logic when a canvas is clicked. 
-- updateCanvasState :: Int -> Game Directive
-- updateCanvasState id = do
--       environment <- ask
--       gameState <- get
--       helper id gameState environment    
--       where
--             helper :: Int -> GameState -> Environment -> Game Directive
--             helper id1 (GameState gameState1) environment1 
--                                                             | gameState1.open == false = do
--                                                                   put $ GameState {open : true, pairs : id1 }                                                            
--                                                                   pure {act: Half, id1: Tuple id1 (findColour environment1 id1), id2: Tuple 0 Blue}
--                                                             | gameState1.open == true = do
--                                                                   let id2 = getPartner environment1 id1
--                                                                   if (gameState1.pairs == id2)
--                                                                         then do
--                                                                               put $ GameState {open : false, pairs : 0 }
--                                                                               pure {act: Pair, id1: Tuple id1 (findColour environment1 id1), id2: Tuple id2 (findColour environment1 id2)}
--                                                                   else  do 
--                                                                         -- launchAff_ do
--                                                                         --       delay $ Milliseconds 2000.0
--                                                                         put $ GameState {open : false, pairs : 0 }
--                                                                         pure {act: Clear, id1: Tuple id1 Blue, id2: Tuple id2 Blue}                                                                 
--             helper  _ _ _ = pure {act: Half, id1: Tuple 0 Blue, id2: Tuple 0 Blue}

-- updateCanvasColour :: Directive -> Number -> Number -> Effect Unit 
-- updateCanvasColour {act: Half, id1: Tuple id1 colour1, id2: Tuple _ __} width height = do
--                                                                         let ctx = getCanvasElt $ id1 -1
--                                                                         colourCanvas ctx colour1 width height
-- updateCanvasColour {act: Pair, id1: Tuple _ __, id2: Tuple id2 colour2} width height = do
--                                                                         let ctx = getCanvasElt $ id2 -1
--                                                                         colourCanvas ctx colour2 width height
-- updateCanvasColour {act: Clear, id1: Tuple id1 _, id2: Tuple id2 __} width height = do
--                                                             let ctx1 = getCanvasElt $ id1 -1
--                                                             let ctx2 = getCanvasElt $ id2 -1
--                                                             resetCanvas ctx1 ctx2 width height

--This is the main logic when a canvas is clicked. It decides what parameters to be 
canvasLogic :: Int -> Number -> Number -> Game (Effect Unit)
canvasLogic id width height = do
      environment <- ask
      gameState <- get
      let ctx = getCanvasElt $ id -1
      helper id ctx width height gameState environment    
      where
            helper :: Int -> Effect Context2D -> Number -> Number -> GameState -> Environment -> Game (Effect Unit)
            helper id1 ctx1 width1 height1 (GameState gameState1) environment1 
                                                            | gameState1.open == false = do
                                                                  put $ GameState {open : true, pairs : id1 }
                                                                  pure $ colourCanvas ctx1 (findColour environment1 id1) width1 height1
                                                            | gameState1.open == true = do
                                                                  if (gameState1.pairs == getPartner environment1 id1)
                                                                        then do
                                                                              put $ GameState {open : false, pairs : 0 }                                                                              
                                                                              pure $ colourCanvas ctx1 (findColour environment1 id1) width1 height1
                                                                  else  do 
                                                                        -- launchAff_ do
                                                                        --       delay $ Milliseconds 2000.0
                                                                        let ctx2 = getCanvasElt $ gameState1.pairs -1
                                                                        put $ GameState {open : false, pairs : 0 }
                                                                        pure $ resetCanvas ctx1 ctx2 width1 height1
                                                      
            helper  _ _ _ _ _ _ = pure $ throwException $ error "Something went wrong in Canvas Logic"
