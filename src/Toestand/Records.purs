-- | Utilities for generating focused boxes from boxes that contain records
module Toestand.Records
  ( useFocusedFields, useFocusedFields' -- useful
  , class UseFocusedFields, useFocusedFieldsImpl -- impl bullshit
  ) where

import Prelude (identity, pure, (<$>), (<*>), (<<<))
import Reactix as R
import Record as Record
import Record.Builder as RB
import Toestand.Boxes (Box, useFocused)
import Toestand.Classes (class ReadWrite)
import Type.Proxy (Proxy(..))
import Type.RowList as RowList
import Typisch.Row (class Cons, class Lacks)

type Builder base row = RB.Builder (Record base) (Record row)

-- | Given a Box (or other ReadWrite) which points at a record, create
-- | a new record where each field is another Box focused over the
-- | corresponding field in the original record.
useFocusedFields
  :: forall l out base box c
   . RowList.RowToList c l
  => ReadWrite box (Record c)
  => UseFocusedFields l out base box
  => box -> Record base -> R.Hooks (Record out)
useFocusedFields box base =
  (\b -> RB.build b base) <$> useFocusedFieldsImpl (Proxy :: Proxy l) box

-- | Builder-returning variant of `useFocusedFields`
useFocusedFields'
  :: forall l out base box c
   . RowList.RowToList c l
  => ReadWrite box (Record c)
  => UseFocusedFields l out base box
  => box -> R.Hooks (RB.Builder (Record base) (Record out))
useFocusedFields' = useFocusedFieldsImpl (Proxy :: Proxy l)

class UseFocusedFields (list :: RowList.RowList Type) (out :: Row Type) (base :: Row Type) box
  | list -> out base box
  where useFocusedFieldsImpl :: Proxy list -> box -> R.Hooks (Builder base out)

instance nilUseFocusedFields :: UseFocusedFields RowList.Nil base base box where
  useFocusedFieldsImpl _ _ = pure identity

instance consUseFocusedFields ::
  ( ReadWrite box (Record c)
  , Cons label (Box a) out' out
  , Cons label a b c
  , Lacks label base
  , UseFocusedFields tail out' base box
  ) => UseFocusedFields (RowList.Cons label a tail) out base box where
  useFocusedFieldsImpl _ box = step <$> useFocus <*> rest
    where
      rest :: R.Hooks (Builder base out')
      rest = useFocusedFieldsImpl (Proxy :: Proxy tail) box
      step :: Box a -> Builder base out' -> Builder base out
      step box' next = (RB.insert labelP box') <<< next
      labelP = Proxy :: Proxy label
      useFocus :: R.Hooks (Box a)
      useFocus = useFocused read write box where
        read :: Record c -> a
        read = Record.get labelP
        write :: a -> Record c -> Record c
        write = Record.set labelP
