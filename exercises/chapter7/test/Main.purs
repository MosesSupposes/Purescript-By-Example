module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)

import Data.AddressBook (examplePerson)
import Data.AddressBook.Validation (validatePerson)

main :: Effect Unit
main = logShow (validatePerson examplePerson)
