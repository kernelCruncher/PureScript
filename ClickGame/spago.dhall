{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "console"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "partial"
  , "prelude"
  , "promises"
  , "psci-support"
  , "random"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
