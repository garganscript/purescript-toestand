module Test.Main where

import Prelude (Unit, discard)
import Data.Traversable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT)
import Test.Spec.Mocha ( runMocha )

-- import DOM.Simple.Document.Spec as Document
-- import DOM.Simple.Element.Spec as Element
-- import DOM.Simple.Parent.Spec as Parent

spec :: SpecT Aff Unit Effect Unit
spec = sequence_ []

main :: Effect Unit
main = runMocha spec
