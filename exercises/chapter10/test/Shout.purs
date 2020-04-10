module Test.Shout where

import Prelude

shout :: forall a. Show a => a -> String
shout a = show a <> "!!!"