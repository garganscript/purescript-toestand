module Toestand.Classes
  ( class Read, class Write, class ReadWrite
  , read, listen, write, write_, modify, modify_
  ) where

import Prelude (Unit, flip, void, ($), (<$>), (>>=))
import Effect (Effect)
import Reactix (class MonadDelay)
import Toestand.Changes (Listener)

-- | A `Box`-like container that can be read from
class Read box val | box -> val where
  -- | Read the current value. Will never cause a refresh.
  read :: forall m. MonadDelay m => box -> m val
  -- | Be called back whenever something changes
  listen :: Listener val -> box -> Effect (Effect Unit)

-- | A `Box`-like container that can be written to.
class Write box val | box -> val where
  -- | Write a new value into the box.
  write :: val -> box -> Effect val

-- | A shorter way of specify Read+Write
class (Read box val, Write box val) <= ReadWrite box val | box -> val

instance readWrite :: (Read box val, Write box val) => ReadWrite box val

-- | Unit-returning version of `write`
write_ :: forall box v. Write box v => v -> box -> Effect Unit 
write_ v box = void $ write v box

-- | Change the value in a Box by applying a function to it.
modify :: forall c v. ReadWrite c v => (v -> v) -> c -> Effect v
modify f box = (f <$> read box) >>= (flip write) box

-- | Change the value in a Box by applying a function to it.
modify_ :: forall box v. ReadWrite box v => (v -> v) -> box -> Effect Unit
modify_ f box = void $ modify f box
