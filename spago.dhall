{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "canvas"
  , "console"
  , "effect"
  , "js-timers"
  , "lists"
  , "partial"
  , "psci-support"
  , "undefined"
  , "web-events"
  , "web-html"
  , "web-uievents"
  , "foreign-generic"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
