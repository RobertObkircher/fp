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

-- complexity O(sqrt n)
sumOfDivisors :: Nat1 -> Nat1
sumOfDivisors 1 = 1
sumOfDivisors n = sum $ map divisorValue $ takeWhile (\x -> x*x <= n) [1..]
  where
    divisorValue :: Nat1 -> Nat0
    divisorValue x
      | r /= 0 = 0
      | q*q > n && q /= n = x + q
      | otherwise = x
      where (q,r) = quotRem n x

-- creates a lazy list as the return value
folge :: Nat1 -> [Nat1]
folge n = n : go [n]
  where
    go :: [Nat1] -> [Nat1]
    go done@(last:_) = let s = sumOfDivisors last
              in if s `elem` done
                   then []
                   else s : go (s:done)

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

