{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "canvas"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "math"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "random"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
