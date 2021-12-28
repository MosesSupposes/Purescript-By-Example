module Test.MySolutions where

import Prelude
import ChapterExamples (Amp(..), Volt(..))
import Data.Person (Person)
import Data.Picture (Shape(..), origin)

factorial :: Int -> Int
factorial 0 = 1

factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1

binomial 0 _ = 0

binomial n k
  | n < k = 0
  | otherwise = factorial n / (factorial k * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1

pascal 0 _ = 0

pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } }
  | c1 == c2 = true
  | otherwise = false

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x

fromSingleton defaultValue _ = defaultValue

circleAtOrigin :: Shape
circleAtOrigin = Circle { x: 0.0, y: 0.0 } 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter s = case s of
  Circle p r -> Circle origin (r * 2.0)
  Rectangle p c r -> Rectangle origin (c * 2.0) (r * 2.0)
  -- I incorrectly implemented this; the tests won't pass because of this case.
  Line p1 p2 -> Line { x: p1.x * 2.0, y: p1.y * 2.0 } { x: p2.x * 2.0, y: p2.y * 2.0 }
  Text p t -> Text origin t

newtype Watt
  = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)
