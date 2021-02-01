-- | Manages a collection of listeners which can be selectively
-- | notified of changes to a value.
module Toestand.Watches (Change, Listener, ShouldNotify, Watches, listen, new, notify, forceNotify) where

import Prelude (Unit, ($), (<$>), (>>=), (+), bind, discard, pure)
import Control.Applicative (when)
import Data.Map as Map
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Ref as Ref

-- | Passed to callbacks when the data changes
type Change a = { new :: a, old :: a }

-- | An Effect function which is provided the new and old values (in that order).
type Listener a = Change a -> Effect Unit

-- | A predicate which determines whether notifications should be sent (true = yes).
type ShouldNotify a = Change a -> Boolean

-- | A collection of listeners.
newtype Watches a = Watches (Ref.Ref (Watches' a))

type Watches' a =
  { nextId       :: Int
  , listeners    :: Map.Map Int (Listener a)
  , shouldNotify :: ShouldNotify a
  }

-- | Create a new Watches from a should notify predicate.
new :: forall a. ShouldNotify a -> Effect (Watches a)
new shouldNotify = Watches <$> Ref.new { nextId: 0, listeners: Map.empty, shouldNotify }

-- | Be notified when the value changes
-- | Callback args: new old
-- | Return: unsubscribe effect
listen :: forall a. Watches a -> Listener a -> Effect (Effect Unit)
listen (Watches ref) f = do
  w <- Ref.read ref
  let listeners = Map.insert w.nextId f w.listeners
  let w' = w { nextId = w.nextId + 1, listeners = listeners }
  Ref.write w' ref
  pure (unlisten ref w.nextId)

unlisten :: forall a. Ref.Ref (Watches' a) -> Int -> Effect Unit
unlisten ref id = do
  w <- Ref.read ref
  let listeners = Map.delete id w.listeners
  let w' = w { listeners = listeners }
  Ref.write w' ref

-- | Notify all listeners if the should notify predicate approves.
notify :: forall a. Watches a -> a -> a -> Effect Unit
notify (Watches ref) new' old = do
    w <- Ref.read ref
    when (w.shouldNotify { new: new', old: old }) (notify' new' old w)

-- | Notify all listeners regardless of whether the should notify predicate approves.
forceNotify :: forall a. Watches a -> a -> a -> Effect Unit
forceNotify (Watches ref) new' old = Ref.read ref >>= notify' new' old

notify' :: forall a. a -> a -> Watches' a -> Effect Unit
notify' new' old w = traverse_ (\f -> f { new: new', old: old }) $ Map.values w.listeners
