{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "toestand"
, dependencies =
  [ "effect"
  , "foldable-traversable"
  , "ordered-collections"
  , "prelude"
  , "reactix"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  , "typisch"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
