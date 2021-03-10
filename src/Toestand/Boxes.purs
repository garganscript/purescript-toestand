module Toestand.Boxes ( Box, useBox, useFocused, useLive ) where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Toestand.Classes (class Read, class Write, class ReadWrite, read, listen, write)
import Toestand.Changes (Listener, ShouldReload, mapChange)
import Toestand.Watches as W

-- import Unsafe.Coerce (unsafeCoerce)

data Box b
  = Root  (R.Ref (Root b))  -- A mutable value plus watchers
  | Focus (R.Ref (Focus b)) -- A lensed view over something Box-like

data Root r = R r (W.Watches r)

newtype Focus f =
  F { read   :: forall m. R.MonadDelay m => Unit -> m f
    , listen :: Listener f -> Effect (Effect Unit)
    , write  :: f -> Effect f }

instance readBox :: Read (Box t) t where
  read (Root ref) = readRoot <$> R.readRefM ref
  read (Focus ref) = R.readRefM ref >>= \f -> readFocus f unit
  listen l (Root ref) = off <$> (R.readRefM ref >>= on) where
    on (R val w) = pure id <* R.setRef ref (R val w') where
      (id /\ w') = W.listen l w
    off id = R.readRefM ref >>= off' where
      off' (R val w) = R.setRef ref (R val $ W.unlisten w id)
  listen l (Focus ref) = R.readRefM ref >>= \(F f) -> f.listen l

readRoot :: forall r. Root r -> r
readRoot (R v _) = v

readFocus :: forall t. Focus t -> forall m. R.MonadDelay m => Unit -> m t
readFocus (F f) u = f.read u

instance writeBox :: Write (Box t) t where
  write new (Root ref) = pure new <* (R.readRefM ref >>= write') where
    write' (R old w) = R.setRef ref (R new w) <* W.notify w { new, old }
  write new (Focus ref) = R.readRefM ref >>= \(F f) -> f.write new

-- | A hook which allocates a Box, a flexible mutable wrapper for
-- | state utilising a react ref
useBox :: forall b. b -> R.Hooks (Box b)
useBox value = Root <$> R.useRef (R value W.watches)

-- | Create a new box by focusing in on the box with accessor functions.
useFocused :: forall box b c. ReadWrite box b
         => (b -> c) -> (c -> b -> b) -> box -> R.Hooks (Box c)
useFocused r w box = Focus <$> R.useRef f where
  f = F { read: read', listen: listen', write: write' }
  read' :: forall m. R.MonadDelay m => Unit -> m c
  read' _ = readWith r box
  listen' l = listenWith l r box
  write' v = read box >>= \c -> writeWith w v c box

readWith :: forall box c v. Read box c
         => (c -> v) -> box
         -> forall m. R.MonadDelay m => m v
readWith r box = r <$> read box

listenWith :: forall box b c. Read box b
           => Listener c -> (b -> c) -> box -> Effect (Effect Unit)
listenWith l r box = listen (l <<< mapChange r) box

writeWith :: forall box b c. ReadWrite box b
          => (c -> b -> b) -> c -> b -> box -> Effect c
writeWith w v c box = pure v <* write (w v c) box

-- | Return the live value of the box. Reload the current component
-- | when it is updated if the provided effectful callback returns true
useLive :: forall box b. Read box b => ShouldReload b -> box -> R.Hooks b
useLive shouldReload box = read box <* (R.useState' 0 >>= uL) where
  uL (_ /\ reload) = R.useEffectOnce $ listen listener box where
    listener change = shouldReload change >>= maybeReload where
      maybeReload check = when check $ reload (_ + 1)
