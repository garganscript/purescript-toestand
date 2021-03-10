module Toestand.Changes
  ( Listener, ShouldReload, Change
  , change, changed, mapChange, unchanged, unequal
  ) where

import Effect (Effect)
import Prelude (class Eq, Unit, notEq, pure)

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
