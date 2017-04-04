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


type State = {  randNum :: Maybe Number
              , loading :: Boolean
              , username :: String
              , result :: Maybe String
              }

data Query a = Regenerate a
             | SetUsername String a
             | MakeRequest a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (random :: RANDOM, ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, username: "", result: Nothing, randNum: Nothing }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [HP.class_ $ H.ClassName "mui-container"] $
      [
        HH.div [HP.class_ $ H.ClassName "mui-panel"] [renderRandomButton state]
      , HH.div [HP.class_ $ H.ClassName "mui-panel"] [renderGithub state      ]
      ]

  renderRandomButton :: State -> H.ComponentHTML Query
  renderRandomButton state =
    let
      value = maybe "No number generated yet" show state.randNum
      interest = maybe "No number generated yet" show (I.calculateInterest `map` state.randNum)
    in
      HH.div_ $
        [ HH.h1_ [ HH.text "Random number" ]
        , HH.p_ [ HH.text ("Current value: " <> value) ]
        , HH.p_ [ HH.text ("Interest: " <> interest )]
        , HH.button
            [ HE.onClick (HE.input_ Regenerate) ]
            [ HH.text "Generate new number" ]
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

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (random :: RANDOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    Regenerate next -> do
      newNumber <- H.liftEff random
      H.modify (_ { randNum = Just newNumber })
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
