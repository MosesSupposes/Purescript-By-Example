module Test.MySolutions where

import Data.Foldable
import Data.Maybe
import Data.Newtype
import Prelude
import Data.Array (nub, nubEq)
import Data.Generic.Rep (class Generic)
import Data.Monoid (power)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

-- Note to reader: Add your solutions to this file
newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) =
    let
      imaginaryIsPostive = imaginary > 0.0
    in
      "" <> show real <> (if imaginaryIsPostive then "+" else mempty) <> show imaginary <> "i"

derive instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  add = over2 Complex add
  mul =
    over2 Complex \{ real: r1, imaginary: i1 } { real: r2, imaginary: i2 } ->
      { real: r1 * r2 - i1 * i2
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

data NonEmpty a
  = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [ e2 ] <> a2)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> " " <> show a1

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

data Extended a
  = Infinite
  | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Infinite) (Finite _) = GT
  compare (Finite _) (Infinite) = LT
  compare (Infinite) (Infinite) = EQ
  compare (Finite x) (Finite y) = compare x y

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr func st (NonEmpty val arr) = foldr func st ([ val ] <> arr)
  foldl func st (NonEmpty val arr) = foldl func st ([ val ] <> arr)
  foldMap func (NonEmpty val arr) = foldMap func ([ val ] <> arr)

data OneMore f a
  = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr func st (OneMore val more) = func val lastB
    where
    lastB = foldr func st more
  foldl func st (OneMore val more) = foldl func firstB more
    where
    firstB = (func st val)
  foldMap func (OneMore val more) = (func val) <> (foldMap func more)

derive instance eqPoint :: Eq Point

derive instance eqShape :: Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordPoint :: Ord Point

derive instance ordShape :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = case maximum xs of
  Just x -> x

class
  Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply
  = Multiply Int

derive newtype instance showMultiply :: Show Multiply

derive newtype instance eqMultiply :: Eq Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply n) m = n * m

instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) s = power s n

instance actionArray :: Action m a => Action m (Array a) where
  act m arr = map (act m) arr

newtype Self m
  = Self m

derive newtype instance showSelf :: Show m => Show (Self m)

derive newtype instance eqSelf :: Eq m => Eq (Self m)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)
