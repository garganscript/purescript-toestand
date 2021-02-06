-- | Manages a collection of listeners which can be selectively
-- | notified of changes to a value.
module Toestand.Watches
  ( Watches(..), watches, listen, notify, unlisten ) where

import Prelude (pure, ($), (+), (<*))
import Data.Map as Map
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Toestand.Types (Change, Listener)

data Watches a = Watches Int (Map.Map Int (Listener a))

-- | Create a new Watches from a should notify predicate.
watches :: forall a. Watches a
watches = Watches 0 Map.empty

-- | Add a change listener effect. Returns a key that can be used to
-- | unlisten along with modified watches.
listen :: forall a. Listener a -> Watches a -> (Tuple Int (Watches a))
listen f (Watches id listeners) =
  Tuple id $ Watches (id + 1) (Map.insert id f listeners)

-- | Remove the provided
unlisten :: forall a. Watches a -> Int -> Watches a
unlisten (Watches id' ls) id = Watches id' (Map.delete id ls)

-- | Call back all the listeners with a change
notify :: forall a. Watches a -> Change a -> Effect (Change a)
notify (Watches _ listeners) change =
  pure change <* traverse_ (_ $ change) (Map.values listeners)
