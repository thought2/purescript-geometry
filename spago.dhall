{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "psci-support"
    , "sized-vectors"
    , "typelevel"
    , "generics-rep"
    , "default"
    , "profunctor-lenses"
    , "math"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
