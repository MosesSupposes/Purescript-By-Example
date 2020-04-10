module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.TestFFI (runGcd, runShout)

main :: Effect Unit
main = do
  log $ show $ runGcd 15 20
  log $ runShout