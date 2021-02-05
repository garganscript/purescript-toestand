-- | The Cell is essentially a mutable value held in a react ref with
-- | some added state management features:
-- |
-- | * Listen for changes to the value.
-- | * Selectively alert listeners when the value has changed.
-- | * Selectively refresh the component when the value changes.
module Toestand.Cells
  (Cell, Cursor, View, useCell, useView, useCursor, useLive, modify) where

import Prelude ((<$>), (>>=), ($), (+), bind, discard, flip, pure, void)
import Control.Applicative (when)
import Effect (Effect)
import Data.Tuple (snd)
import Reactix as R
import Toestand.Classes (class Read, class Write, listen, read, write)
import Toestand.Watches as W
import Toestand.Types (ShouldRefresh)

-- | A flexible mutable wrapper for state based around a react
-- | ref. See module documentation for more.
newtype Cell c = Cell (R.Ref (C c))

data C c = C c (W.Watches c)

-- | A read-only view over a cell.
newtype View cell c v = View (R.Ref (V cell c v))

data V cell c v = V cell (c -> v) (W.Watches v)

-- | A read-write view over a cell
newtype Cursor cell c v = Cursor (R.Ref (D cell c v))

-- C was taken
data D cell c v = D cell (c -> v) (v -> c -> c) (W.Watches v)

-- | A hook which allocates a Cell, a flexible mutable wrapper for
-- | state utilising a react ref
useCell :: forall c. c -> R.Hooks (Cell c)
useCell value = do
  watches <- R.unsafeHooksEffect $ W.watches
  Cell <$> R.useRef (C value watches)

-- | Create a new View from the provided read transform and cell.
useView :: forall cell c v. Read cell c =>
        (c -> v) -> cell -> R.Hooks (View cell c v)
useView read' cell = do
  watches <- R.unsafeHooksEffect $ W.watches
  R.useEffect $ listen (listener watches) cell
  View <$> R.useRef (V cell read' watches)
  where
    listener watches { new, old } =
      void $ W.notify watches { new: read' new, old: read' old }

-- | Create a new cursor from a read transform, a write transform and a cell
useCursor
  :: forall cell c v
   . Read cell c
  => Write cell c
  => (c -> v) -> (v -> c -> c) -> cell -> R.Hooks (Cursor cell c v)
useCursor read' write' cell = do
  watches <- R.unsafeHooksEffect $ W.watches
  R.useEffect $ listen (listener watches) cell
  Cursor <$> R.useRef (D cell read' write' watches)
  where
    listener watches { new, old } =
      void $ W.notify watches { new: read' new, old: read' old }
-- | Return the live value of the cell, scheduling a refresh of the
-- | current component when it is updated if the provided should
-- | refresh predicate returns true
useLive
  :: forall cell val
   . Read cell val
   => ShouldRefresh val -> cell -> R.Hooks val
useLive shouldRefresh cell = do
  refresh <- snd <$> R.useState' 0
  R.useEffect $ listen (listener refresh) cell
  read cell
  where
    listener refresh change = do
      check <- shouldRefresh change
      when check (refresh (_ + 1))

modify :: forall c v. Read c v => Write c v => (v -> v) -> c -> Effect v
modify f cell = (f <$> read cell) >>= (flip write) cell

-- instances

instance readCell :: Read (Cell c) c where
  read (Cell ref) = liftC (\a _ -> a) <$> R.readRefM ref
  listen l (Cell ref) = R.readRefM ref >>= liftC (\_ w -> W.listen l w)

instance writeCell :: Write (Cell c) c where
  write new (Cell ref) = do
    (C old watches) <- R.readRefM ref
    R.setRef ref (C new watches)
    (_.new) <$> W.notify watches { new, old }

instance readView :: Read cell c => Read (View cell c v) v where
  read (View ref) =
    R.readRefM ref >>= liftV (\cell read' _ -> read' <$> read cell)
  listen l (View ref) =
    R.readRefM ref >>= liftV (\_ _ w -> W.listen l w)

instance readCursor :: Read cell c => Read (Cursor cell c v) v where
  read (Cursor ref) =
    R.readRefM ref >>= liftD (\cell read' _ _ -> read' <$> read cell)
  listen l (Cursor ref) =
    R.readRefM ref >>= liftD (\_ _ _ w -> W.listen l w)

instance writeCursor :: (Read cell c, Write cell c) => Write (Cursor cell c v) v where
  write new (Cursor ref) = do
    (D cell _ write' watches) <- R.readRefM ref
    all <- read cell
    let x = write' new all
    _ <- write x cell
    pure new

liftC :: forall b c. (c -> W.Watches c -> b) -> C c -> b
liftC f (C a b) = f a b

liftV :: forall c c' v b. (c -> (c' -> v) -> W.Watches v -> b) -> V c c' v -> b
liftV f (V a b c) = f a b c

liftD
  :: forall c c' v b
   . (c -> (c' -> v) -> (v -> c' -> c') -> W.Watches v -> b) -> D c c' v -> b
liftD f (D a b c d) = f a b c d
