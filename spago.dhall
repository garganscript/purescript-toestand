{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "toestand"
, dependencies =
  [ "aff"
  , "arrays"
  , "dom-simple"
  , "effect"
  , "ffi-simple"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "reactix"
  , "record"
  , "refs"
  , "spec"
  , "spec-mocha"
  , "tuples"
  , "typelevel-prelude"
  , "typisch"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
