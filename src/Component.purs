module Component where 

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Maybe (Maybe(..), maybe)
import Network.HTTP.Affjax as AX
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Interest (calculateInterest) as I
import Helper (muiContainer, muiPanel)

type State = {  randNum :: Maybe Number
              , loading :: Boolean
              , username :: String
              , result :: Maybe String
              , buttonState :: Boolean
              }

shouldRender :: State -> Boolean
shouldRender st = case tmp of 
                    Just true -> true 
                    _         -> false
  where tmp = goodEnough `map` st.randNum
        goodEnough :: Number -> Boolean 
        goodEnough n = n > 0.5

data AddQuery a = Add Number Number a 

data Query a = Regenerate a
             | SetUsername String a
             | MakeRequest a

data Message = Toggled Boolean   
             | RerenderMap     

type Input = Unit     

ui :: forall eff. H.Component HH.HTML Query Input Message (Aff (random :: RANDOM, ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, username: "", result: Nothing, randNum: Nothing, buttonState: false }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [HP.class_ muiContainer] $
      [ renderGoogleMap state
      , HH.div [HP.class_ muiPanel] [renderRandomButton state]
      , HH.div [HP.class_ muiPanel] [renderGithub state      ]
      ]

  renderGoogleMap st = if tmp then HH.div [HP.id_ "map"] [] else HH.div [] []
    where tmp = shouldRender st

  renderRandomButton :: State -> H.ComponentHTML Query
  renderRandomButton state =
    let
      label = if state.buttonState then "On" else "Off"
      value = maybe "No number generated yet" show state.randNum
      interest = maybe "No number generated yet" show (I.calculateInterest `map` state.randNum)
    in
      HH.div_ $
        [ HH.h1_ [ HH.text "Random number" ]
        , HH.p_ [ HH.text ("Current value: " <> value) ]
        , HH.p_ [ HH.text ("Interest: " <> interest )]
        , HH.button
            [ HE.onClick (HE.input_ Regenerate) ]
            [ HH.text "Generate new number"]
        ]

  renderGithub :: State -> H.ComponentHTML Query
  renderGithub st =
    HH.div_ $
      [ HH.h1_ [ HH.text "Lookup GitHub user" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter username:" ]
          , HH.input
              [ HP.value st.username
              , HE.onValueInput (HE.input SetUsername)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeRequest)
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
          case st.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (random :: RANDOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    Regenerate next -> do      
      newNumber <- H.liftEff random
      state <- H.get
      H.modify (_ { randNum = Just newNumber, buttonState = not state.buttonState })
      H.raise $ Toggled state.buttonState
      H.raise $ RerenderMap 
      pure next
    SetUsername username next -> do 
      H.modify (_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do 
      username <- H.gets _.username 
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get ("https://api.github.com/users/" <> username)
      H.modify (_ { loading = false, result = Just response.response })
      pure next
