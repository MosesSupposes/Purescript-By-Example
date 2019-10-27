module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.Path (root)
import Effect (Effect)
import Effect.Console (logShow)
import FileOperations (allFiles)

main :: Effect Unit
main = for_ (allFiles root) logShow
