module Initialisation where

import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error, throwException)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Effect.Console (logShow)

-- I think the <- only works with the same monad. This is why we can't have a <- 1 ..9 and b <- getCanvasElementById ("canvas"<> show a) in the same method.
-- This is why I've split functionality into two functions.
getCanvasElt :: Int -> Effect Context2D
getCanvasElt subsc = do
            let name = "canvas"<> show subsc
            logShow name
            context <- getCanvasElementById (name)
            case context of
                Just x ->  getContext2D x
                Nothing -> throwException $ error ("Canvas Element not found. Id must be between 1 and 8: " <> name)

getCanvasEltArray :: Array (Effect Context2D)
getCanvasEltArray  = do 
            index <- 1 .. 8
            pure $ getCanvasElt index


-- The below was my first attempt to create an InitialState. It failed because in the foldL I was using a random generator which produces type Effect. Hwever the foldL was expecting an Array type to be produced because 
--the initialiser was an empty array, []. It didn't know how to handle Effect Array; indeed how can you append matrices in this format.
-- createInitialState :: Effect GameState
-- createInitialState = do
--       pure $ foldl (\acc x -> do
--             randomNumbers <- getTwoRandomNumbers(acc)
--             acc <> [panelStateCreator randomNumbers x]) [] [Blue, Green, Red, Yellow]
--             where 
--                   panelStateCreator :: Array Int -> Colour -> PanelState
--                   panelStateCreator [a,b] colour = {colour: colour, numb1: a, numb2: b}
--                   panelStateCreator _ _= {colour: Blue, numb1: 1, numb2: 1} -- this line doesn't matter.


-- --This was/is a headache. We need to create pairs fo random numbers, but without repetition.
-- getTwoRandomNumbers:: Array PanelState-> Effect (Array Int)
-- getTwoRandomNumbers [] = unsafePartial do 
--             i <- randomInt 1 8
--             jIndex <- randomInt 0 6
--             let Just j = (filter (\x -> x /= i) (1..8) !! jIndex)
--             pure [i,j]
-- getTwoRandomNumbers xs = unsafePartial do
--             let lengthOfList = length xs
--             let availableIndices = 8 - (2 * lengthOfList)
--             randomIndexOne <- randomInt 0 availableIndices
--             randomIndexTwo <- randomInt 0 $ availableIndices - 1
--             let existingNumberArray = concatMap (\x -> do
--                                     let x1 = x.numb1
--                                     let x2 = x.numb2
--                                     [x1, x2]) xs
--             let freeNumbers = do
--                         x <- 1..8
--                         y <- existingNumberArray
--                         guard $ x /= y
--                         pure x
--             let Just i = freeNumbers !! randomIndexOne
--             let freeNumbersMinusi = filter (\x -> x /= i) freeNumbers
--             let Just j = freeNumbersMinusi !! randomIndexTwo
--             pure [i, j]

--Still to do.
--Need to write a function that listens to all Canvas clicks. When a canvas is clicked, it colours the canvas the colour described in the Initial State. 
--It checks if another canvas tile is open. If so, it compares the the two. If they are not the same they are covered again, otherwise they are left open.