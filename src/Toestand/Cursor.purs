-- | A read-write view over a cell. Features:
-- |
-- | * Modify the perceibed contents with a function when reading.
-- | * Modify the whole cell's value when writing.
-- | * Selectively refresh the component.
module Toestand.Cursor ( Cursor, View, useCursor, useView ) where

import Prelude
  ( class Applicative, Unit, bind, pure, unit
  , (<$>), (>>=), (<<<), (<*) )
import Effect (Effect)
import Reactix as R
import Toestand.Types

-- | A read-only view over part of a cell.
newtype View v = View (R.Ref (View' v))

instance readView :: Read (View v) v where
  read (View ref)     = do
    u <- fix unit
    let dict = _monadDelayDict u
    v <- R.readRefM ref
    _read dict u v
  listen l (View ref) = R.readRefM ref >>= _listen l

useView
  :: forall cell c v. Read cell c
  => (c -> v) -> cell -> R.Hooks (View v)
useView r cell = View <$> R.useRef (_view read' listen') where
  dict = _readDict cell
  read' = readWith dict r cell
  listen' l = listenWith dict l r cell

-- | Opaque wrapper over the inside of a View
foreign import data View' :: Type -> Type

foreign import _view
  :: forall read listen value. read -> listen -> View' value

type ReadDict cell c =
  { read :: forall m. MonadDelayDict -> m Unit -> cell -> m c
  , listen :: Listener c -> cell -> Effect (Effect Unit)}

foreign import _readDict
  :: forall cell c. Read cell c => cell -> ReadDict cell c

foreign import data MonadDelayDict :: Type

foreign import _monadDelayDict :: forall m. R.MonadDelay m => m Unit -> MonadDelayDict

foreign import _read :: forall m cell c. MonadDelayDict -> m Unit -> cell -> m c

readWith
  :: forall cell c v. Read cell c 
  => ReadDict cell c
  -> (c -> v)
  -> cell
  -> MonadDelayDict
  -> forall m. Applicative m => m v
readWith dict r cell delay = r <$> dict.read delay (pure unit) cell

foreign import _listen :: forall l cell. cell -> l -> Effect (Effect Unit)

listenWith :: forall cell c v. ReadDict cell c -> Listener v -> (c -> v) -> cell -> Effect (Effect Unit)
listenWith dict l r cell = dict.listen (l <<< mapChange r) cell


---- Cursor

-- | Opaque wrapper over the inside of a Cursor
foreign import data Cursor' :: Type -> Type

-- | Create a new cursor over a cell from a pair of read and write
-- | functions in the van Laarhoven lens style.
useCursor
  :: forall cell c v. ReadWrite cell c
  => (c -> v) -> (v -> c -> c) -> cell -> R.Hooks (Cursor v)
useCursor r w cell = Cursor <$> R.useRef (_cursor read' listen' write') where
  readDict = _readDict cell
  writeDict = _writeDict cell
  read' = readWith readDict r cell
  listen' l = listenWith readDict l r cell
  write' v = read cell >>= \c -> writeWith writeDict w v c cell

writeWith
  :: forall cell c v
   . ReadWrite cell c
  => WriteDict cell c -> (v -> c -> c) -> v -> c -> cell -> Effect v
writeWith dict w v c cell = pure v <* dict.write new cell
  where new = w v c

-- | A read-write view over part of a cell
newtype Cursor c = Cursor (R.Ref (Cursor' c))

instance readCursor :: Read (Cursor c) c where
  read (Cursor ref)     = do
    u <- fix unit
    let dict = _monadDelayDict u
    v <- R.readRefM ref
    _read dict u v

  listen l (Cursor ref) = R.readRefM ref >>= _listen l
-- instance readCursor :: Read (Cursor v) v where
--   read (Cursor ref)     = do
--     v <- R.readRefM ref
--     _read (pure unit)
--   listen l (Cursor ref) = R.readRefM ref >>= _listen l

instance writeCursor :: Write (Cursor v) v where
  write new (Cursor ref) = R.readRefM ref >>= _write new



type WriteDict cell c = { write :: c -> cell -> Effect c }

foreign import _writeDict
  :: forall cell c. Write cell c => cell -> WriteDict cell c

foreign import _cursor
  :: forall read listen write value. read -> listen -> write -> Cursor' value

foreign import _write :: forall cell v. v -> cell -> Effect v

fix :: forall m a. R.MonadDelay m => a -> m (m a)
fix = pure <<< pure
