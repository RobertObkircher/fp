module Angabe2 where

import Data.List(sort, foldl')
import Control.Applicative((<$>)) -- hugs...

type Nat0 = Integer
type Nat1 = Integer

-- the double primes in a range
dp :: (Nat1, Nat1) -> [Nat1]
dp (a,b) = filter isDp [min a b .. max a b]

-- an infinite list of prime numbers
primes :: [Nat1]
primes = filter isPrime' [1..]
  where
    isPrime' :: Nat1 -> Bool
    isPrime' 1 = False
    isPrime' 2 = True
    isPrime' n = all (\x -> n `rem` x /= 0) $ takeWhile (\x -> x^2 <= n) primes

-- check if a number is prime
isPrime :: Nat0 -> Bool
isPrime n = head (dropWhile (\x -> x < n && n `rem` x /= 0) primes) == n

-- is the number a double prime
isDp :: Nat1 -> Bool
isDp x = isPrime x && isPrime (reverseNat x)

-- reverses the decimal representation of a non negative number
reverseNat :: Nat0 -> Nat0
reverseNat = read . reverse . show


{- Example

primeFactors = 2 2 2 3 3

sum = 
    1 + 2 + 4 + 8 +
    1 + 3 + 9

-}
sumOfDivisors :: Nat1 -> Nat1
sumOfDivisors 1 = 1
sumOfDivisors n = product $ (uncurry sumOfPowers) <$> (countAdjacent $ primeFactors n)
  where
    factors = primeFactors n

-- 1 + p + p^2 + ... + p^k
sumOfPowers :: Nat1 -> Nat0 -> Nat1
sumOfPowers 1 _ = 1
sumOfPowers p k = (p^(k + 1) - 1) `div` (p - 1)

countAdjacent :: Eq a => [a] -> [(a, Nat1)]
countAdjacent [] = []
countAdjacent l@(x:_) = let (ys, zs) = span (==x) l
                          in (x, fromIntegral (length ys)) : countAdjacent zs

primeFactors :: Nat1 -> [Nat1]
primeFactors n
  | isPrime n = [n]
  | otherwise = factorize n $ takeWhile (\x -> 2*x <= n) primes
    
factorize :: Nat1 -> [Nat1] -> [Nat1]
factorize _ [] = []
factorize n l@(x:xs)
  | n `rem` x /= 0 = factorize n xs
  | otherwise = x : factorize (n `div` x) l

folge :: Nat1 -> [Nat1]
folge n = reverse $ go [n]
  where
    go :: [Nat1] -> [Nat1]
    go l@(1:_) = l
    go l@(x:_) = let s = sumOfDivisors x - x
                  in if s `elem` l
                       then l
                       else go (s:l)

-- the medianoid if the list contains unique integers
-- otherwise the sum of the list is returned
medianoid :: [Int] -> Int
medianoid = medianoidSorted . sort 

-- same as medianoid but requires the list to be sorted
medianoidSorted :: [Int] -> Int
medianoidSorted [] = 0
medianoidSorted m
  | adjacentDuplicates m = sum m
medianoidSorted m = m !! (length m `div` 2)

-- are two adjaced elements the same
adjacentDuplicates :: [Int] -> Bool
adjacentDuplicates [] = False
adjacentDuplicates xs = or $ zipWith (==) xs (tail xs) -- or of [] is false

