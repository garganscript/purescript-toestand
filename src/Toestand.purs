-- | A simple way of managing a tree of state in react while retaining
-- | full control over rerendering.
module Toestand
  ( module Toestand.Changes
  , module Toestand.Classes
  , module Toestand.Boxes
  , module Toestand.Records
  ) where

import Toestand.Boxes ( Box, useBox, useFocused, useLive )
import Toestand.Changes
  ( Listener, ShouldReload, Change
  , change, changed, mapChange, unchanged, unequal )
import Toestand.Classes
  ( class Read, class Write, class ReadWrite
  , read, listen, write, write_, modify, modify_ )
import Toestand.Records (useFocusedFields, useFocusedFields')
