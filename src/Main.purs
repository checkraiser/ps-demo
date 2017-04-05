module Main where 

import Prelude
import Control.Monad.Eff (Eff)
import Control.Coroutine as CR
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Halogen.Aff as HA
import Network.HTTP.Affjax as AX
import Halogen.VDom.Driver (runUI)
import Control.Monad.Eff.Random (RANDOM)
import Component as UI

main :: Eff (HA.HalogenEffects (random :: RANDOM, ajax :: AX.AJAX, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI UI.ui unit body

  io.subscribe $ CR.consumer \(UI.Toggled newState) -> do
    log $ "Button was toggled to: " <> show newState
    pure Nothing
