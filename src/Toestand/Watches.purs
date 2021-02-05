-- | Manages a collection of listeners which can be selectively
-- | notified of changes to a value.
module Toestand.Watches
  ( Watches, watches, listen, notify ) where

import Prelude (Unit, ($), (<$>), (>>=), (+), bind, discard, pure)
import Control.Applicative (when)
import Data.Map as Map
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Ref as Ref
import Toestand.Types (Change, Listener)

-- | A collection of listeners.
newtype Watches a = Watches (Ref.Ref (W a))

data W a = W Int (Map.Map Int (Listener a))

-- | Create a new Watches from a should notify predicate.
watches :: forall a. Effect (Watches a)
watches = Watches <$> Ref.new (W 0 Map.empty)

-- | Add a change listener effect. Returns an unsubscribe effect
listen :: forall a. Listener a -> Watches a -> Effect (Effect Unit)
listen f (Watches ref) = do
  (W id listeners) <- Ref.read ref
  let new = W (id + 1) (Map.insert id f listeners)
  Ref.write new ref
  pure (unlisten ref id)

-- Creates an unlistener effect for a listener
unlisten :: forall a. Ref.Ref (W a) -> Int -> Effect Unit
unlisten ref id = do
  (W id' listeners) <- Ref.read ref
  let new = W id' (Map.delete id listeners)
  Ref.write new ref

-- | Call back all the listeners with a change
notify :: forall a. Watches a -> Change a -> Effect (Change a)
notify (Watches ref) change = do
  (W _ listeners) <- Ref.read ref
  traverse_ (_ $ change) (Map.values listeners)
  pure change
