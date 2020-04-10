module Test.TestFFI where

foreign import runGcd :: Int -> Int -> Int
foreign import runShout :: String