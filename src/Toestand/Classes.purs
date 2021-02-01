module Toestand.Classes
  ( class Read, read, listen
  , class Write, write
  , modify
  ) where

import Prelude (Unit, (<$>), (>>=), flip)
import Effect (Effect)
import Reactix (class MonadDelay)
import Toestand.Cell as Cell
import Toestand.Cursor as Cursor
import Toestand.View as View
import Toestand.Watches (Listener)

class Read c v where
  read :: forall m. MonadDelay m => c -> m v
  listen :: c -> (Listener v) -> Effect (Effect Unit)
  -- view   :: forall w. (v -> w) -> View 

class Write c v where
  write :: v -> c -> Effect v

modify :: forall c v. Read c v => Write c v => (v -> v) -> c -> Effect v
modify f cell = (f <$> read cell) >>= (flip write) cell

instance readCell :: Read (Cell.Cell c) c where
  read   = Cell.read
  listen = Cell.listen
  -- view   = View.view

instance readCursor :: Read (Cursor.Cursor c v) v where
  read   = Cursor.read
  listen = Cursor.listen
  -- view   = Cursor.view

instance readView :: Read (View.View c v) v where
  read   = View.read
  listen = View.listen
  -- view   = View.subview

instance writeCell :: Write (Cell.Cell c) c where
  write = Cell.write
  -- cursor = Cursor.cursor

instance writeCursor :: Write (Cursor.Cursor c v) v where
  write = Cursor.write
  -- cursor = Cursor.subcursor
