module Toestand.Types
  ( Change, Listener, ShouldRefresh, change, changed, unchanged) where

import Prelude (Unit)
import Effect (Effect)

-- | A summary of a change in value
type Change c = { new :: c, old :: c }

-- | An Effect function which is provided the new and old values.
type Listener c = Change c -> Effect Unit

-- | An effect function which determines whether notifications should be sent.
type ShouldRefresh c = Change c -> Effect Boolean

-- | Create a change positionally. new first.
change :: forall a b. a -> a -> Change a
change new old = { new, old }

-- | Adapt a positional function to a change function. new first.
changed :: forall a b. (a -> a -> b) -> Change a -> b
changed f { new, old } = f new old

-- | Adapt a change function to a positional function. new first.
unchanged :: forall a b. (Change a -> b) -> a -> a -> b
unchanged f new old = f { new, old }
