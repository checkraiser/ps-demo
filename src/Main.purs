module Main where 

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Network.HTTP.Affjax as AX
import Halogen.VDom.Driver (runUI)
import Control.Monad.Eff.Random (RANDOM)
import Component (ui)

main :: Eff (HA.HalogenEffects (random :: RANDOM, ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body