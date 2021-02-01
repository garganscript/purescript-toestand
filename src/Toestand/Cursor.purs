-- | A read-write view over a cell. Features:
-- |
-- | * Modify the perceibed contents with a function when reading.
-- | * Modify the whole cell's value when writing.
-- | * Selectively refresh the component.
module Toestand.Cursor (Cursor, cursor, listen, read, subcursor, useCursor, view, write) where

import Prelude (Unit, ($), (<$>), (<<<), (+), bind, const, discard)
import Control.Applicative (when)
import Data.Tuple (snd)
import Effect (Effect)
import Reactix as R
import Toestand.Cell as Cell
import Toestand.View as View
import Toestand.Watches (Listener)

-- | A read-write view over a cell
newtype Cursor c v = Cursor
  { read  :: c -> v
  , write :: v -> c -> c
  , cell  :: Cell.Cell c }

-- | Create a new cursor from a read transform, a write transform and a cell
cursor :: forall c v. (c -> v) -> (v -> c -> c) -> Cell.Cell c -> Cursor c v
cursor read' write' cell = Cursor { read: read', write: write', cell }

subcursor :: forall c v w. (v -> w) -> (w -> v -> v) -> Cursor c v -> Cursor c w
subcursor read' write' (Cursor c) = Cursor $ { read: read'', write: write'', cell: c.cell }
  where read'' = read' <<< c.read
        write'' :: w -> c -> c
        write'' w all = c.write (write' w $ c.read all) all

-- | Get a read-only view of the cursor
view :: forall c v. Cursor c v -> View.View c v
view (Cursor c) = View.view c.read c.cell

-- | Get the current value of the cursor and a writer function.
useCursor :: forall c v. Cell.ShouldRefresh v -> Cursor c v -> R.Hooks v
useCursor shouldRefresh c'@(Cursor c) = do
  refresh <- snd <$> R.useState' 0
  R.useEffect do
    Cell.listen c.cell $ \change ->
      when (shouldRefresh' change) (refresh (_ + 1))
  read c'
  where
    shouldRefresh' {new, old} = shouldRefresh {new: c.read new, old: c.read old}

-- | Read the current value of the Cursor. Can be called in either Hooks or Effet
read :: forall m c v. R.MonadDelay m => Cursor c v -> m v
read (Cursor c) = c.read <$> Cell.read c.cell

-- | Write a new value into the cursor, irrespective of the current value.
write :: forall c v. v -> Cursor c v -> Effect v
write value (Cursor c) = do
  all <- Cell.read c.cell
  const value <$> Cell.write (c.write value all) c.cell

-- | Run an Effectful function when a change occurs. Returns a cancel effect.
listen :: forall c v. Cursor c v -> (Listener v) -> Effect (Effect Unit)
listen (Cursor c) l = Cell.listen c.cell $ \{new, old} -> l {new: c.read new, old: c.read old}
