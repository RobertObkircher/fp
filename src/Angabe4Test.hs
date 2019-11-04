module Angabe4Test
    ( module Test
    , module Angabe4
    , module Angabe4Test
    ) where

import Test
import Angabe4

test_all = do
    test tc_a1
    test tc_a2
    test tc_a4'2
    test tc_binaer
    print test_conversion
    testRange "plus" test_plus
    testRange "minus" test_minus
    testRange "mal" test_mal
    testRange "durch" test_durch
    testRange "gleich" test_gleich
    testRange "ungleich" test_ungleich
    testRange "groesser" test_groesser
    testRange "kleiner" test_kleiner
    testRange "ggleich" test_ggleich
    testRange "kgleich" test_kgleich
    testRange "a_zu_bin" test_a_zu_binaer
    testRange "bin_zu_a" test_binaer_zu_a

m = Plus Eins :: ZZ
n = Minus Eins :: ZZ

tc_a4'2 =
    [ show Null ->> "0"
    , show (Plus (Nf (Nf (Nf Eins)))) ->> "4"
    , show (Plus (Nf (Nf (Nf (Nf (Nf (Nf (Nf (Nf (Nf Eins)))))))))) ->> "A"
    , show (Minus (Nf (Nf (Nf (Nf (Nf (Nf (Nf (Nf (Nf Eins)))))))))) ->> "-A"
    ]

tc_binaer :: [TestCase]
tc_binaer =
    [ a_zu_binaer Eins ->> "1"
    , a_zu_binaer (Nf Eins) ->> "10"
    , a_zu_binaer (to_IN_1 15) ->> "1111"
    , a_zu_binaer (to_IN_1 16) ->> "10000"
    , a_zu_binaer (to_IN_1 17) ->> "10001"
    , from_IN_1 (binaer_zu_a "1") ->> 1
    , from_IN_1 (binaer_zu_a "10") ->> 2
    , from_IN_1 (binaer_zu_a "1111") ->> 15
    , from_IN_1 (binaer_zu_a "10000") ->> 16
    , from_IN_1 (binaer_zu_a "10001") ->> 17
    , from_IN_1 (binaer_zu_a "0") ->> 1
    , from_IN_1 (binaer_zu_a "-1") ->> 1
    , from_IN_1 (binaer_zu_a "-10") ->> 1
    , from_IN_1 (binaer_zu_a "-1111") ->> 1
    , from_IN_1 (binaer_zu_a "-10000") ->> 1
    , from_IN_1 (binaer_zu_a "-10001") ->> 1
    , a_zu_binaer Null ->> "0"
    , a_zu_binaer (0 :: Zett) ->> "0"
    , a_zu_binaer (Plus Eins) ->> "1"
    , a_zu_binaer (1 :: Zett) ->> "1"
    , a_zu_binaer (Minus Eins) ->> "-1"
    , a_zu_binaer ((-1) :: Zett) ->> "-1"
    ]

tc_a1 =
    [ von_Zett_nach_ZZ 0 ->> Null
    , von_Zett_nach_ZZ 2 ->> Plus (Nf Eins)
    , von_ZZ_nach_Zett (Minus (Nf (Nf Eins))) ->> (-3)
    , von_ZZ_nach_Zett Null ->> 0
    ]

tc_a2 =
    [ plus m n ->> Null
    , minus m n ->> Plus (Nf Eins)
    , Plus (Nf (Nf Eins)) `mal` Minus (Nf Eins) ->> Minus (Nf (Nf (Nf (Nf (Nf Eins)))))
    , Plus (Nf (Nf (Nf (Nf (Nf (Nf Eins)))))) `durch` Plus (Nf (Nf Eins)) ->> Plus (Nf Eins)
    , Plus (Nf (Nf (Nf (Nf (Nf (Nf Eins)))))) `durch` Minus (Nf (Nf Eins)) ->> Minus (Nf Eins)
    , durch (Plus Eins) Null ->> Null
    ]

range :: [Zett]
range = [-20..20]


test_conversion :: Bool
test_conversion = map convert range == range
  where
    convert = von_ZZ_nach_Zett . von_Zett_nach_ZZ

testRange :: String -> (Zett -> Zett -> Bool) -> IO ()
testRange msg f = putStrLn $ msg ++ if checkRange f then " OK" else " FAIL"


checkRange :: (Zett -> Zett -> Bool) -> Bool
checkRange f = and [f a b | a<-range, b<-range]

test_a_zu_binaer :: Zett -> Zett -> Bool
test_a_zu_binaer n _ = a == b
  where
    a = a_zu_binaer (n :: Zett)
    b = a_zu_binaer (von_Zett_nach_ZZ n :: ZZ)

test_binaer_zu_a :: Zett -> Zett -> Bool
test_binaer_zu_a n _ = a == b
  where
    a = (binaer_zu_a bin :: ZZ)
    b = fromInteger (binaer_zu_a bin :: Zett)
    bin = a_zu_binaer n


test_plus :: Zett -> Zett -> Bool
test_plus a b = plus a' b' == result
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b
    result = von_Zett_nach_ZZ (a + b)

test_minus :: Zett -> Zett -> Bool
test_minus a b = minus a' b' == result
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b
    result = von_Zett_nach_ZZ (a - b)

test_mal :: Zett -> Zett -> Bool
test_mal a b = mal a' b' == result
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b
    result = von_Zett_nach_ZZ (a * b)

-- https://stackoverflow.com/q/24209927
divEuclidean :: Int -> Int -> Int
x `divEuclidean` y = q + yNeg
  where
    (q,r) = (x + xNeg) `quotRem` y
    xNeg = fromEnum (x < 0)
    yNeg = xNeg*(2 * fromEnum (y < 0) - 1)

divEuclidean' :: Integer -> Integer -> Integer
divEuclidean' a b = toInteger (divEuclidean (fromInteger a) (fromInteger b))

test_durch :: Zett -> Zett -> Bool
test_durch a b 
  | durch a' b' == result = True
  | otherwise = error $ show a ++ " " ++ show b ++ " -> " ++ show (durch a' b')
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b
    result = von_Zett_nach_ZZ $ if b /= 0 then a `divEuclidean'` b else 0

test_gleich :: Zett -> Zett -> Bool
test_gleich a b = gleich a' b' == (a == b)
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b

test_ungleich :: Zett -> Zett -> Bool
test_ungleich a b = ungleich a' b' == (a /= b)
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b

test_groesser :: Zett -> Zett -> Bool
test_groesser a b = groesser a' b' == (a > b)
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b

test_kleiner :: Zett -> Zett -> Bool
test_kleiner a b = kleiner a' b' == (a < b)
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b

test_ggleich :: Zett -> Zett -> Bool
test_ggleich a b = ggleich a' b' == (a >= b)
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b

test_kgleich :: Zett -> Zett -> Bool
test_kgleich a b = kgleich a' b' == (a <= b)
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b

