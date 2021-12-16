module Test.MySolutions where

import Prelude
import Test.Examples

import Control.Alternative (guard)
import Data.Array (filter, head, tail, (..), cons, (:))
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Path (Path(..), isDirectory, ls, filename, size)
import Data.Semigroup.Foldable (foldl1)
import Data.Maybe

isEven :: Int -> Boolean
isEven num = num `mod` 2 == 0

countEven :: Array Int -> Int
countEven nums = case nums of
  [] -> 0
  _ ->
    let
      x = fromMaybe 0 (head nums)

      xs = fromMaybe [] (tail nums)
    in
      if isEven x then 1 + countEven xs else countEven xs

squared :: Array Number -> Array Number
squared xs = map (\x -> x * x) xs

keepNonNegative :: Array Number -> Array Number 
keepNonNegative ns = filter (\n -> n >= 0.0) ns 

infixl 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite ns = (\n -> n >= 0.0) <$?> ns

isPrime :: Int -> Boolean
isPrime x = (not $ x `eq` 1) && (length $ factors x) == 1

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct left right = do
  a_ <- left
  b_ <- right
  pure [ a_, b_ ]


triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k
  pure [ i, j, k ]

primeFactors :: Int -> Array Int
primeFactors n = factorize 2 n
  where
  factorize :: Int -> Int -> Array Int
  factorize _ 1 = []
  factorize divisor dividend =
    if dividend `mod` divisor == 0 then
      cons divisor $ factorize (divisor) (dividend / divisor)
    else
      factorize (divisor + 1) dividend


allTrue :: Array Boolean -> Boolean
allTrue = foldl (\acc bool -> acc && bool) true

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) n2 (n1 + n2)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x]  <> xs ) []

onlyFiles :: Path -> Array Path
onlyFiles path = filter (not isDirectory) (allFiles path)

whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ do
  path' <- allFiles path
  child <- ls path'
  guard $ filename child == filename path' <> fileName
  pure path'

largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path) 
  where
    loop :: Array Path -> Path -> Array Path
    loop [largest, smallest] current | size current < size smallest = [largest, current]
                                     | size current > size largest  = [current, smallest]
                                     | otherwise                    = [largest, smallest]
    loop [last] current              | size current < size last     = [current, last]
                                     | otherwise                    = [last, current]
    loop arr current                                                = current : arr
