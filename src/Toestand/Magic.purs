-- | Utilities for generating cursors and views from cells (or cursors
-- | or views...) that contain records.
module Toestand.Magic
  ( class FieldCursors, class FieldViews, fieldCursors, fieldViews
  , useFieldCursors, useFieldCursors', useFieldViews, useFieldViews' ) where

import Prelude
import Reactix as R
import Record as Record
import Record.Builder as RB
import Toestand.Cells as T
import Toestand.Types (class Read, class ReadWrite)
import Type.Prelude (RLProxy(..), SProxy(..))
import Type.RowList as RowList
import Typisch.Row (class Cons, class Lacks)

type Builder base row = RB.Builder (Record base) (Record row)

-- | Given a Cell (etc.) which points at a record, create a new record
-- | where each field is a Cursor over the corresponding field in the
-- | Cell.
useFieldCursors
  :: forall l out base cell c
   . RowList.RowToList c l
  => ReadWrite cell (Record c)
  => FieldCursors l out base cell
  => cell -> Record base -> R.Hooks (Record out)
useFieldCursors cell base
  =   (\b -> RB.build b base)
  <$> fieldCursors (RLProxy :: RLProxy l) cell

-- | Builder-returning variant of `useFieldCursors`
useFieldCursors'
  :: forall l out base cell c
   . RowList.RowToList c l
  => ReadWrite cell (Record c)
  => FieldCursors l out base cell
  => cell -> R.Hooks (RB.Builder (Record base) (Record out))
useFieldCursors' = fieldCursors (RLProxy :: RLProxy l)

class FieldCursors (list :: RowList.RowList) (out :: # Type) (base :: # Type) cell
  | list -> out base cell
  where fieldCursors :: RLProxy list -> cell -> R.Hooks (Builder base out)

instance nilFieldCursors :: FieldCursors RowList.Nil base base cell where
  fieldCursors _ _ = pure identity

instance consFieldCursors ::
  ( ReadWrite cell (Record c)
  , Cons label (T.Cursor cell (Record c) a) out' out
  , Cons label a b c
  , Lacks label base
  , FieldCursors tail out' base cell
  ) => FieldCursors (RowList.Cons label a tail) out base cell where
  fieldCursors _ cell = step <$> useCursor <*> rest
    where
      rest :: R.Hooks (Builder base out')
      rest = fieldCursors (RLProxy :: RLProxy tail) cell
      step :: T.Cursor cell (Record c) a -> Builder base out' -> Builder base out
      step cursor next = (RB.insert labelP cursor) <<< next
      labelP = SProxy :: SProxy label
      useCursor :: R.Hooks (T.Cursor cell (Record c) a)
      useCursor = T.useCursor read write cell where
        read :: Record c -> a
        read = Record.get labelP
        write :: a -> Record c -> Record c
        write = Record.set labelP

-- | Like `useFieldCursors`, but for views instead of cursors.
useFieldViews
  :: forall list out base cell c
   . RowList.RowToList c list
  => Read cell (Record c)
  => FieldViews list out base cell
  => cell -> Record base -> R.Hooks (Record out)
useFieldViews cell base = (\b -> RB.build b base) <$> fieldViews list cell
  where 
    list = RLProxy :: RLProxy list

-- | Builder-returning variant of `useFieldViews`
useFieldViews'
  :: forall l out base cell c
   . RowList.RowToList c l
  => Read cell (Record c)
  => FieldViews l out base cell
  => cell -> R.Hooks (RB.Builder (Record base) (Record out))
useFieldViews' = fieldViews (RLProxy :: RLProxy l)

class FieldViews (list :: RowList.RowList) (out :: # Type) (base :: # Type) cell
  | list -> out base cell
  where fieldViews :: RLProxy list -> cell -> R.Hooks (Builder base out)

instance nilFieldViews :: FieldViews RowList.Nil base base cell where
  fieldViews _ _ = pure identity

instance consFieldViews ::
  ( Read cell (Record c)
  , Cons label (T.View cell (Record c) a) out' out
  , Cons label a b c
  , Lacks label base
  , FieldViews tail out' base cell
  ) => FieldViews (RowList.Cons label a tail) out base cell where
  fieldViews _ cell = step <$> useView <*> rest
    where
      rest :: R.Hooks (Builder base out')
      rest = fieldViews (RLProxy :: RLProxy tail) cell
      step :: T.View cell (Record c) a -> Builder base out' -> Builder base out
      step view next = (RB.insert labelP view) <<< next
      labelP = SProxy :: SProxy label
      useView :: R.Hooks (T.View cell (Record c) a)
      useView = T.useView read cell where
        read :: Record c -> a
        read = Record.get labelP
