module Test.MySolutions where

import Data.Newtype
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- Note to reader: Add your solutions to this file
newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point {x,y}) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = 
    let imaginaryIsPostive = imaginary > 0.0 
    in "" <> show real <> (if imaginaryIsPostive then "+" else mempty) <> show imaginary <> "i"

derive instance eqComplex :: Eq Complex 

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  add = over2 Complex add
  mul = over2 Complex
          \ { real: r1, imaginary: i1 }
            { real: r2, imaginary: i2 }
          ->
            { real:      r1 * r2 - i1 * i2
            , imaginary: r1 * i2 + r2 * i1
            }
  zero = wrap zero
  one = wrap { real: one, imaginary: zero }

derive newtype instance ringComplex :: Ring Complex 

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow