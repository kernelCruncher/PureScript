module Main where

import Prelude

import CSS (Color, backgroundColor)
import CanvasComponent (findColour, getPartner)
import Color.Scheme.Clrs (blue, red, green, gray, yellow)
import Data.Array ((!!), modifyAt)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (class MonadEffect)
import GameEnvironment (Colour(..), Environment, createInitialEnv)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State = {open :: Int, 
              env :: Environment,
              but :: Boolean, 
              colours ::  Array Color}

data Action = Play | SelectColour Int

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = {open: 0, 
                  env: [],
                  but: true,
                  colours: replicate 8 gray }

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ HH.div_
      [ HH.div_ 
              [HH.button [HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 0, HE.onClick \_ -> SelectColour 1 ] [HH.text "Button 1" ]
              ,HH.button [HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 1, HE.onClick \_ -> SelectColour 2 ] [HH.text "Button 2" ]
              ]
      , HH.div_ 
              [HH.button [HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 2, HE.onClick \_ -> SelectColour 3 ] [HH.text "Button 3" ]
              , HH.button[HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 3, HE.onClick \_ -> SelectColour 4 ] [HH.text "Button 4" ]
              ]
      , HH.div_ 
              [HH.button [HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 4, HE.onClick \_ -> SelectColour 5 ] [HH.text "Button 5" ]
              , HH.button[HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 5, HE.onClick \_ -> SelectColour 6 ] [HH.text "Button 6" ]
              ]
      , HH.div_  
              [HH.button [HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 6, HE.onClick \_ -> SelectColour 7 ] [HH.text "Button 7" ]
              , HH.button[HP.disabled state.but, CSS.style $ backgroundColor $ justColor $ state.colours !! 7, HE.onClick \_ -> SelectColour 8 ] [HH.text "Button 8" ]
              ]
      ],
    HH.div_
      [HH.button [ HE.onClick \_ -> Play ] [ HH.text "Start Game" ]]
  ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Play -> do
    newEnv <- H.liftEffect createInitialEnv
    H.modify_ \state -> state {env = newEnv,
                              but = false, 
                              colours = replicate 8 gray}
  SelectColour i -> do
    environment <- H.gets _.env
    colours <- H.gets _.colours
    let colour =  findColour environment i
    let newColours = modifyAt (i-1) (\_-> colorConverter colour) colours 
    H.modify_ \state -> state {colours = maybeColors newColours}
    open <- H.gets _.open
    case open of
      0 -> do
        H.modify_ \state -> state {open = i}
      x -> do
        let partner = getPartner environment i
        if (x == partner)
          then 
            H.modify_ \state -> state {open = 0}
        else 
            H.modify_ \state -> state {  
                              open = 0,
                              colours = replicate 8 gray}

justColor :: Maybe Color -> Color
justColor (Just x) = x
justColor Nothing = gray

colorConverter :: Colour -> Color
colorConverter Blue = blue
colorConverter Red = red
colorConverter Green = green
colorConverter Yellow = yellow

maybeColors :: Maybe (Array Color) -> Array Color
maybeColors (Just x) = x
maybeColors Nothing = replicate 8 gray

-- sort out grey on buttons
-- make buttons bigger HP.width $ CSSPixel 80,
-- Modal saying you win
-- refactor code to be less repetitive (how do you access field in record using variable, e.g. button1)
-- buttons deactivated