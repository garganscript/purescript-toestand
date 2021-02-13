module Toestand.Types
  ( class Read, class Write, class ReadWrite, read, listen, write
  , Listener, ShouldReload, Change
  , change, changed, mapChange, unchanged, unequal, modify
  ) where

import Prelude
import Effect (Effect)
import Reactix (class MonadDelay)

-- | A `Cell`-like container that can be read from
class Read cell val | cell -> val where
  -- | Read the current value. Will never cause a refresh.
  read :: forall m. MonadDelay m => cell -> m val
  -- | Be called back whenever something changes
  listen :: Listener val -> cell -> Effect (Effect Unit)

-- | A `Cell`-like container that can be written to.
class Write cell val | cell -> val where
  -- | Write a new value into the cell.
  write :: val -> cell -> Effect val

class (Read cell val, Write cell val) <= ReadWrite cell val | cell -> val

instance readWrite :: (Read cell val, Write cell val) => ReadWrite cell val

-- | A summary of a change in value
type Change c = { new :: c, old :: c }

-- | An Effect function which is provided the new and old values.
type Listener c = Change c -> Effect Unit

-- | An effect function which determines whether notifications should be sent.
type ShouldReload c = Change c -> Effect Boolean

-- | Create a change positionally. new first.
change :: forall a. a -> a -> Change a
change new old = { new, old }

-- | Adapt a positional function to a change function. new first.
changed :: forall a b. (a -> a -> b) -> Change a -> b
changed f { new, old } = f new old

-- | Transform a `Change` by applying a function to both old and new values.
mapChange :: forall c d. (c -> d) -> Change c -> Change d
mapChange f { new, old } = { new: f new, old: f old }

-- | Adapt a change function to a positional function. new first.
unchanged :: forall a b. (Change a -> b) -> a -> a -> b
unchanged f new old = f { new, old }

-- | An equality based refresh predicate
unequal :: forall v. Eq v => Change v -> Effect Boolean
unequal { new, old } = pure (new `notEq` old)

-- | Change the value in a Cell by applying a function to it.
modify :: forall c v. ReadWrite c v => (v -> v) -> c -> Effect v
modify f cell = (f <$> read cell) >>= (flip write) cell
