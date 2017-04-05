module GoogleMap where 

import Prelude (Unit)
import Control.Monad.Eff

foreign import data GOOGLE :: !
foreign import initMap :: forall eff. Eff (google :: GOOGLE | eff) Unit