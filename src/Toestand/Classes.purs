module Toestand.Classes ( class Read, read, listen, class Write, write ) where

import Prelude (Unit)
import Effect (Effect)
import Reactix (class MonadDelay)
import Toestand.Types (Listener)

class Read cell val where
  -- | Read the current value. Will never cause a refresh.
  read :: forall m. MonadDelay m => cell -> m val
  -- | Be called back whenever something changes
  listen :: Listener val -> cell -> Effect (Effect Unit)

class Write cell val where
  -- | Write a new value into the cell.
  write :: val -> cell -> Effect val
