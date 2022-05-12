let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220510/packages.dhall sha256:0b0d4db1f2f0acd3b37fa53220644ac6f64cf9b5d0226fd097c0593df563d5be

let overrides = {=}

let additions =
      { dom-simple =
        { dependencies =
          [ "effect"
          , "ffi-simple"
          , "maybe"
          , "nullable"
          , "prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/garganscript/purescript-dom-simple"
        , version = "ps-15.0-upgrade"
        }
      , ffi-simple =
        { dependencies =
          [ "functions"
          , "maybe"
          , "nullable"
          , "prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/garganscript/purescript-ffi-simple"
        , version = "v0.3.2"
        }
      , reactix =
        { dependencies =
          [ "dom-simple"
          , "effect"
          , "ffi-simple"
          , "foldable-traversable"
          , "functions"
          , "maybe"
          , "nullable"
          , "prelude"
          , "strings"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/garganscript/purescript-reactix"
        , version = "v0.5.0"
        }
      , typisch =
          { dependencies =
            [ "prelude" ]
          , repo = "https://github.com/garganscript/purescript-typisch"
          , version = "v0.3.0"
        }
      , inflection =
          { dependencies =
            [ "functions" ]
          , repo = "https://github.com/athanclark/purescript-inflection"
          , version = "v1.0.0"
        }
      , spec-mocha =
          { dependencies =
            [ "console", "foldable-traversable", "exceptions", "spec" ]
          , repo = "https://github.com/purescript-spec/purescript-spec-mocha"
          , version = "v4.0.0"
        }
      }

in  upstream // overrides // additions
