module Test.Main where

import Prelude (Unit, discard)
import Data.Traversable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (SpecT)
import Test.Spec.Mocha ( runMocha )

import Toestand.Cell.Spec as Cell
-- import Toestand.Cursor.Spec as Cursor

spec :: SpecT Aff Unit Effect Unit
spec = sequence_ [ Cell.spec ]

main :: Effect Unit
main = runMocha spec
