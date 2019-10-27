module Angabe1 where

-- import Data.Bool (bool)
import Data.Bits ((.&.))
import Control.Applicative((<*>)) -- for some reason this is not available in hugs

-- Must not be negative
type Nat0 = Int

-- Removes every nth occurence of a character.
-- If i<=0 the original string is returned.
streiche :: String -> Int -> Char -> String
streiche = streiche' 1

-- Same as streiche. The first param must be 1
-- if i is <= 0 we will simply rebuild the same list
streiche' :: Int -> String -> Int -> Char -> String
streiche' _ [] i c = []
streiche' nth (x:xs) i c
  | nth == i && x == c = streiche' 1 xs i c -- skip x
  | otherwise = x : streiche' (nth + if x /= c then 0 else 1) xs i c

{-
streiche s i c = snd $ foldr (\x (nth, res) ->
    if nth == i && c == x
        then (1, res)
        else (if c == x then nth + 1 else nth, x : res)
    ) (1, "") s
-}

{-
streiche :: String -> Int -> Char -> String
streiche s i c = snd $ foldr (\x (nth, res) ->
      bool (nth + bool 0 1 (c == x), x : res) (1, res) (nth == i && c == x)
    ) (1, "") s
-}

-- Is the decimal representation a reversed power of two?
ist_umgekehrt_2er_potenz :: Nat0 -> Bool
ist_umgekehrt_2er_potenz = powOf2 . read . reverse . show

-- Is it a power of two?
powOf2 :: Nat0 -> Bool
powOf2 0 = False
powOf2 n = (n .&. (n-1)) == 0
--  | n `mod` 2 == 0 = powOf2 (n `div` 2)
--  | otherwise = n == 1

-- Returns either the largest palindrome (decimal representation) or -1
groesstes_palindrom_in :: [Nat0] -> Int
groesstes_palindrom_in ns = maximum $ (-1) : filter palindrom ns

-- A somewhat inefficient check if the decimal representatio is a palindrome
palindrom :: Nat0 -> Bool
palindrom = ((==) <*> reverse) . show

