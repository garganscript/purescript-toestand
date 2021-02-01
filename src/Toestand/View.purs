-- | A read-only view over a cell. Features:
-- |
-- | * Modify the returned contents with a function when reading.
-- | * Selectively refresh the component.
module Toestand.View (View, listen, read, subview, useView, view) where

import Prelude (Unit, ($), (<$>), (<<<), (+), bind, discard)
import Data.Tuple (snd)
import Control.Applicative (when)
import Effect (Effect)
import Reactix as R
import Toestand.Cell as Cell
import Toestand.Watches (Listener)

-- | A read-only view over a cell.
newtype View c v = View
  { read :: c -> v
  , cell :: Cell.Cell c }

-- | Create a new View from the provided read transform and Cell.
view :: forall c v. (c -> v) -> Cell.Cell c -> View c v
view read' cell = View { read: read', cell }

-- | Creates a new view that adds an extra read transform over this one.
subview :: forall c v w. (v -> w) -> View c v -> View c w
subview f (View v) = View { read: read', cell: v.cell }
  where read' = f <<< v.read

-- | Return the current value of the View. Will refresh the component
-- | when the should refresh predicate returns true.
useView :: forall c v. Cell.ShouldRefresh v -> View c v -> R.Hooks v
useView shouldRefresh (View v) = do
  refresh <- snd <$> R.useState' 0
  R.useEffect do
    Cell.listen v.cell $ \change ->
      when (shouldRefresh' change) (refresh (_ + 1))
  v.read <$> Cell.read v.cell
  where
    shouldRefresh' {new, old} = shouldRefresh {new: v.read new, old: v.read old}

-- | Read the current value of the View. Can be called in either Hooks or Effet
read :: forall m c v. R.MonadDelay m => View c v -> m v
read (View v) = v.read <$> Cell.read v.cell

-- | Run an Effectful function when a change occurs. Returns a cancel effect.
listen :: forall c v. View c v -> (Listener v) -> Effect (Effect Unit)
listen (View v) l = do
  Cell.listen v.cell $ \{new, old} -> l {new: v.read new, old: v.read old}
