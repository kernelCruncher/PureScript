module GameEnvironment where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Data.Array ((..), (!!), filter, length)
import Effect.Console (logShow)
import Data.Foldable (foldl)
import Effect.Random (randomInt)
import Data.Maybe (Maybe(..))

data Colour =  Blue | Green | Red | Yellow

--The derive instance is just for debugging purposes.
derive instance genericColour :: Generic Colour _

instance showColour:: Show Colour where
    show = genericShow

type PanelState = { colour :: Colour, numb1 :: Int, numb2 :: Int}
type Environment = Array PanelState

--Here we create the initial State of the game. This will need to go into the call to Play.
createInitialEnv :: Effect Environment
createInitialEnv = do
            array <- createInitialStateRecursive(1..8)
            logShow array
            let arrayPair = arrayPairer array
            logShow arrayPair
            let colours = [Blue, Green, Red, Yellow]
            let gameState = foldl (\acc x -> unsafePartial do
                  let [a,b] = extractNumber arrayPair x
                  let colour = extractNumber colours x
                  acc <> [{ colour : colour, numb1 : a, numb2 : b}]) [] (0..3)
            pure gameState

arrayPairer :: Array Int -> Array (Array Int)
arrayPairer xs = map (\i -> unsafePartial do
    let Just x = (xs !! (2*i))
    let Just y = (xs !! (2*i+1))
    [x,y]) (0..((length xs)/2-1))

createInitialStateRecursive :: Array Int -> Effect (Array Int)
createInitialStateRecursive array | length array ==0 = pure []
                                  | otherwise = do
                                          randomNumbers <- getRandomPair array
                                          let first = unsafePartial extractNumber randomNumbers 0
                                          let second = unsafePartial extractNumber randomNumbers 1
                                          let reducedList = filter (\x -> x/= first && x/= second) array
                                          tail <- createInitialStateRecursive reducedList
                                          pure $ randomNumbers <> tail
extractNumber:: forall a. Partial => Array a -> Int -> a
extractNumber [x, y] i| i==0 = x
                        | i==1 = y
extractNumber [a,b,c,d] i | i ==0 = a
                            | i == 1 = b
                            | i == 2 = c
                            | i ==3 = d

getRandomPair :: Array Int-> Effect (Array Int)
getRandomPair [] = pure []
getRandomPair xs = unsafePartial do
            let lengthOfList = length xs
            randomIndexOne <- randomInt 0 $ lengthOfList - 1
            randomIndexTwo <- randomInt 0 $ lengthOfList - 2
            let Just i = xs !! randomIndexOne
            let freeNumbersMinusi = filter (\x -> x /= i) xs
            let Just j = freeNumbersMinusi !! randomIndexTwo
            pure [i, j]
