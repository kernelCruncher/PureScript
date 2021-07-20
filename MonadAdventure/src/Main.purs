module Main where

import Prelude

import Control.Monad.RWS (RWSResult(..), runRWS)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Data.GameState (GameState, initialGameState)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Console (log)
import Game (game)
import Node.ReadLine as RL
import Node.Yargs.Applicative (Y, runY, flag, yarg)
import Node.Yargs.Setup (usage)

--The front-end of our game is built using two packages: yargs, which provides an applicative interface to the yargs command line parsing library, 
--and node-readline, which wraps NodeJS' readline module, allowing us to write interactive console-based applications.
runGame :: GameEnvironment -> Effect Unit
runGame env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " interface

  let
    lineHandler :: GameState -> String -> Effect Unit
    lineHandler currentState input = do
      case runRWS (runExceptT (game (split (wrap " ") input))) env currentState of  -- runRWS looks like a combination of runReader, runWriter and runState. It takes a global configuration and an initial state as an argument, and returns a data structure containing the log, the result and the final state.
        RWSResult state action written -> do -- the action is of type Eiter String Unit
          case action of
            Left error -> log error-
            Right _ -> do
              for_ written log --The for_ action is used to traverse the log (of type List String) and print its entries to the console. - Having run the game logic, which is a pure computation, we need to print any log messages to the screen and show the user a prompt for the next command. 
          RL.setLineHandler (lineHandler state) $ interface --Finally, setLineHandler is used to update the line handler function to use the updated game state, and the prompt is displayed again using the prompt action.
      RL.prompt interface -- When the user types the next line it will be using the 
      pure unit

  RL.setLineHandler (lineHandler initialGameState) interface -- this starts the whole process.
  RL.prompt interface -- it's kind of recursive. The Set:LineHandler just keeps getting re-set with different 

  pure unit


--yargs is an example of applicative command line option parsing. 
--Recall that an applicative functor allows us to lift functions of arbitrary arity over a type constructor representing some type of side-effect. 
--In the case of the yargs package, the functor we are interested in is the Y functor, which adds the side-effect of reading from command line options. It provides the following handler:
--runY :: forall a. YargsSetup -> Y (Effect a) -> Effect a. Here, (usage "$0 -p <player name>")  gives YargsSetup and  map runGame env gives Y (Effect Unit)

--Here, the gameEnvironment function, which has the type PlayerName -> Boolean -> GameEnvironment, is lifted over Y. 
--The two arguments specify how to read the player name and debug flag from the command line options. 
--The first argument describes the player name option, which is specified by the -p or --player options, and the second describes the debug mode flag, which is turned on using the -d or --debug options.
--This demonstrates two basic functions defined in the Node.Yargs.Applicative module: yarg, which defines a command line option which takes an optional argument (of type String, Number or Boolean), and flag which defines a command line flag of type Boolean.
--Notice how we were able to use the notation afforded by the applicative operators to give a compact, declarative specification of our command line interface.
-- In addition, it is simple to add new command line arguments, simply by adding a new function argument to runGame, and then using <*> to lift runGame over an additional argument in the definition of env.
main :: Effect Unit
main = runY (usage "$0 -p <player name>") $ map runGame env -- The map takes the runGame, which is of type GameEnvrionment, to Effect Unit and lifts it over the Y monad (as env is of type Y GameEnvironment) to give a Y Effect Unit. The runY needs this type.
  where
  env :: Y GameEnvironment
  env = gameEnvironment -- takes two paramters: PlayerName and Boolean. We need to lift the gameEnvironment function into the Y Applicative functor.
          <$> yarg "p" ["player"] -- Recall <$> is an alias for map. Map takes a function from a -> b as an argument and a constructor type as a second argument.
                       (Just "Player name") -- yarg takes a single command-line argument with parameters: The key name and default argument name, any aliases which can be used in place of the key, an optional description, either a default value or a message to show if this field is required, whether or not an associated value is required
                       (Right "The player name is required")
                       false
          <*> flag "d" ["debug"] -- Recall <$> is an alias for apply. Map and apply can be used to lift a function of an arbitrary number of arguments. Apply takes f(a->b) as an argument to give (f a -> f b)
                       (Just "Use debug mode")
          <*> flag  "c" ["cheatMode"]
                        (Just "Use cheat mode")
