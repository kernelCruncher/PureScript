{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "colors"
  , "console"
  , "css"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "promises"
  , "psci-support"
  , "random"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
