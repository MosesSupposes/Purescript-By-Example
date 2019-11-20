let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191025/packages.dhall sha256:f9eb600e5c2a439c3ac9543b1f36590696342baedab2d54ae0aa03c9447ce7d4

let overrides =
      { form-urlencoded = upstream.form-urlencoded // { version = "v4.0.0" } }

let additions =
      { affjax =
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
          , repo =
              "https://github.com/slamdata/purescript-affjax.git"
          , version =
              "v9.0.1"
          }
      }

in  upstream // overrides // additions
