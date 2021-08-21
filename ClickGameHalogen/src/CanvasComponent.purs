module CanvasComponent where

import Prelude
import GameEnvironment (Colour(..), Environment)

createColour :: Colour -> String
createColour Blue = "#00f"
createColour Green = "#080"
createColour Red = "#F00"
createColour Yellow = "#fa0"

findColour :: Environment -> Int -> Colour
findColour [a,b,c,d] canvasInt | a.numb1 == canvasInt || a.numb2 == canvasInt = a.colour 
                            | b.numb1  == canvasInt || b.numb2 == canvasInt = b.colour
                            | c.numb1 == canvasInt ||  c.numb2 == canvasInt = c.colour
                            | d.numb1  == canvasInt || d.numb2 == canvasInt = d.colour 
findColour _ _ = Blue

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