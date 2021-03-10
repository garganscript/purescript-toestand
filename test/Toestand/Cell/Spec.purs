module Toestand.Cell.Spec where

import Prelude

import DOM.Simple.Element as Element
import DOM.Simple.Types (Element)
import Data.Array ((!!))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence_, traverse_)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FFI.Simple (delay)
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.Test as RT
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Toestand as T

type Spec = SpecT Aff Unit Effect Unit

staticTest :: Spec
staticTest =
  describe "Basic DOM rendering" $ do
    it "Simple elements" $ do
      root <- liftEffect $ RT.render simple
      let children = Element.children root.container
      (Element.name <$> children) `shouldEqual` ["I"]
      (Element.innerHTML <$> children) `shouldEqual` ["hello world"]
    it "Magic props" $ do
      root <- liftEffect $ RT.render magic
      let children = Element.children root.container
      A.length children `shouldEqual` 1
      let children2 = children >>= Element.children
      let attrNames = A.sort (children >>= Element.attrNames)
      let attrVals =
            do name <- attrNames
               child <- children
               fromMaybe $ Element.attr child name
      ["aria-label", "data-sample"] `shouldEqual` attrNames
      ["example", "example"] `shouldEqual` attrVals
    it "Fragments" $ do
      root <- liftEffect $ RT.render $ frag
      Element.childCount root.container `shouldEqual` 2
      let children = Element.children root.container
      A.length children `shouldEqual` 2
      (Element.name <$> children) `shouldEqual` ["I", "I"]
      (Element.innerHTML <$> children) `shouldEqual` ["hello","world"]
   where
     simple = H.i {} [ H.text "hello world" ]
     magic = H.div {aria: {label: "example"}, "data": {sample: "example"}} []
     frag = H.i {} [ H.text "hello" ] <> H.i {} [ H.text "world" ]

spec :: Spec
spec = sequence_ [ staticTest ]
