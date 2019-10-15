{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "argonaut-core"
    , "console"
    , "debug"
    , "effect"
    , "foreign"
    , "psci-support"
    , "random"
    , "web-html"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
