
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20191005/packages.dhall sha256:ba287d858ada09c4164792ad4e643013b742c208cbedf5de2e35ee27b64b6817

let overrides = {
    form-urlencoded = 
        upstream.form-urlencoded // { version = "v4.0.0" }
}

let additions = {
   affjax =
    { dependencies =
        [ "arraybuffer-types"
        , "media-types"
        , "form-urlencoded"
        , "argonaut-core"
        , "aff"
        , "web-xhr"
        , "unsafe-coerce"
        , "refs"
        , "integers"
        , "math"
        , "foreign"
        , "http-methods"
        , "nullable"
        ]
    , repo = "https://github.com/slamdata/purescript-affjax.git"
    , version = "v9.0.1"
    } 
}

in  upstream // overrides // additions
