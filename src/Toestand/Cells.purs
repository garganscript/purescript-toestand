-- | The `Cell` is an 'ideal' state container, a *stable* *reference*
-- | to a value. With this handle, you may read from or write to the
-- | container, as well as subscribe to have an Effect triggered when
-- | a write is performed.
-- | 
-- | The `Cursor` is similar, representing a read-write view over a
-- | part of a `Cell`. To achieve this, it takes a source `Cell` and a
-- | pair of read/write functions in the van Laarhoven lens style.
-- |
-- | Like a `Cursor`, the `View`, operates over part of a `Cell`,
-- | except it is read-only. It takes a source `Cell` and a reader
-- | function in the van Laarhoven lens style.
-- |
-- | These types provide a *stable reference* semantic: they will not
-- | change for prop diffing purposes if the value inside changes.
-- |
-- | Loosely speaking, you allocate a `Cell` containing a structured
-- | piece of state and then create `View`s and `Cursor`s to different
-- | parts of it, passing them down to your contained components.
-- |
-- | The `useLive` hook allows for easy control over component
-- | reloading. You pass in a predicate to determine whether to
-- | trigger a reload, it returns the current value.
module Toestand.Cells
  ( Cell, Cursor, View, useCell, useCursor, useView, useLive ) where

import Prelude ((<$>), (>>=), (<<<), (<*), ($), (+), pure)
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

-- | A read-write view over part of a cell
newtype Cursor cell c v = Cursor (R.Ref (C' cell c v))

data C' cell c v = C' cell (c -> v) (v -> c -> c)

-- | Create a new cursor over a cell from a pair of read and write
-- | functions in the van Laarhoven lens style.
useCursor :: forall cell c v. ReadWrite cell c
          => (c -> v) -> (v -> c -> c) -> cell -> R.Hooks (Cursor cell c v)
useCursor r w cell = Cursor <$> R.useRef (C' cell r w)

instance readCursor :: Read cell c => Read (Cursor cell c v) v where
  read (Cursor ref) = R.readRefM ref >>= read'
    where read' (C' cell r _) = r <$> read cell
  listen l (Cursor ref) = R.readRefM ref >>= liste where
    liste (C' cell r _) = listen (l <<< mapChange r) cell

instance writeCursor :: ReadWrite cell c => Write (Cursor cell c v) v where
  write new (Cursor ref) = pure new <* (R.readRefM ref >>= writ) where
    writ (C' cell r w) = read cell >>= wri where
      wri all = write (w new all) cell

-- | A read-only view over part of a cell.
newtype View cell c v = View (R.Ref (V cell c v))

data V cell c v = V cell (c -> v)

-- | Create a new View over a cell with a read function.
useView :: forall cell c v. Read cell c
        => (c -> v) -> cell -> R.Hooks (View cell c v)
useView r cell = View <$> R.useRef (V cell r)

instance readView :: Read cell c => Read (View cell c v) v where
  read (View ref) = R.readRefM ref >>= rea where
    rea (V cell r) = r <$> read cell
  listen l (View ref) = R.readRefM ref >>= on where
    on (V cell r) = listen (l <<< mapChange r) cell

-- | Return the live value of the cell. Reload the current component
-- | when it is updated if the provided effectful callback returns true
useLive :: forall cell c. Read cell c => ShouldReload c -> cell -> R.Hooks c
useLive shouldReload cell = read cell <* (R.useState' 0 >>= uL) where
  uL (_ /\ reload) = R.useEffectOnce $ listen listener cell where
    listener change = shouldReload change >>= maybeReload where
      maybeReload check = when check $ reload (_ + 1)
