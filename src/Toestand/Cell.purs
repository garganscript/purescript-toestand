-- | The `Cell` is an 'ideal' state container, a *stable* *reference*
-- | to a value. With this handle, you may read from or write to the
-- | container, as well as subscribe to have an Effect triggered when
-- | a write is performed.
-- | 
-- | The Cell provides a *stable reference* semantic: it will not
-- | change for prop diffing purposes if the value inside changes.
-- |
-- | The `useLive` hook allows for easy control over component
-- | reloading. You pass in a predicate to determine whether to
-- | trigger a reload, it returns the current value.
module Toestand.Cell ( Cell, useCell, useLive ) where

import Prelude (pure, ($), (+), (<$>), (<*), (>>=))
import Control.Applicative (when)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Toestand.Watches as W
import Toestand.Types

-- | A flexible mutable wrapper for state based around a react
-- | ref. See module documentation for more.
newtype Cell c = Cell (R.Ref (C c))

data C c = C c (W.Watches c)

-- | A hook which allocates a Cell, a flexible mutable wrapper for
-- | state utilising a react ref
useCell :: forall c. c -> R.Hooks (Cell c)
useCell value = Cell <$> R.useRef (C value W.watches)

instance readCell :: Read (Cell c) c where 
  read (Cell ref) = r <$> R.readRefM ref where
    r (C c _) = c
  listen l (Cell ref) = off <$> (R.readRefM ref >>= on) where
    on (C val w) = pure id <* R.setRef ref (C val w') where
      (id /\ w') = W.listen l w
    off id = R.readRefM ref >>= off' where
      off' (C val w) = R.setRef ref (C val $ W.unlisten w id)

instance writeCell :: Write (Cell c) c where
  write new (Cell ref) = pure new <* (R.readRefM ref >>= writ) where
    writ (C old w) = R.setRef ref (C new w) <* W.notify w { new, old }

-- | Return the live value of the cell. Reload the current component
-- | when it is updated if the provided effectful callback returns true
useLive :: forall cell c. Read cell c => ShouldReload c -> cell -> R.Hooks c
useLive shouldReload cell = read cell <* (R.useState' 0 >>= uL) where
  uL (_ /\ reload) = R.useEffectOnce $ listen listener cell where
    listener change = shouldReload change >>= maybeReload where
      maybeReload check = when check $ reload (_ + 1)
