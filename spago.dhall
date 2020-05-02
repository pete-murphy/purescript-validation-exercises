{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-validation-exercises"
, dependencies =
  [ "console"
  , "effect"
  , "email-validate"
  , "generics-rep"
  , "lists"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "read"
  , "strings"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
