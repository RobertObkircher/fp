{-# LANGUAGE StandaloneDeriving #-}

module Angabe3Test
    ( module Test
    , module Angabe3
    , module Angabe3Test
    ) where

import Test
import Angabe3

deriving instance Eq IN_1
deriving instance Eq ZZ

test_all = do
    test tc_a1
    test tc_a2
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

tc_a1 =
    [ von_Zett_nach_ZZ 0 ->> Null
    , von_Zett_nach_ZZ 2 ->> Plus (Nf Eins)
    , von_ZZ_nach_Zett (Minus (Nf (Nf Eins))) ->> (-3)
    , von_ZZ_nach_Zett Null ->> 0
    ]

tc_a2 =
    [ plus m n ->> Null
    , minus m n ->> Plus (Nf Eins)
    , (Plus (Nf (Nf Eins))) `mal` (Minus (Nf Eins)) ->> Minus (Nf (Nf (Nf (Nf (Nf Eins)))))
    , (Plus (Nf (Nf (Nf (Nf (Nf (Nf Eins))))))) `durch` (Plus (Nf (Nf Eins))) ->> Plus (Nf Eins)
    , (Plus (Nf (Nf (Nf (Nf (Nf (Nf Eins))))))) `durch` (Minus (Nf (Nf Eins))) ->> Minus (Nf Eins)
    , durch (Plus Eins) Null ->> Null
    ]

range :: [Zett]
range = [-42..42]


test_conversion :: Bool
test_conversion = map convert range == range
  where
    convert = von_ZZ_nach_Zett . von_Zett_nach_ZZ

testRange :: String -> (Zett -> Zett -> Bool) -> IO ()
testRange msg f = putStrLn $ msg ++ if checkRange f then " OK" else " FAIL"


checkRange :: (Zett -> Zett -> Bool) -> Bool
checkRange f = and [f a b | a<-range, b<-range]

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

test_durch :: Zett -> Zett -> Bool
test_durch a b = durch a' b' == result
  where
    a' = von_Zett_nach_ZZ a
    b' = von_Zett_nach_ZZ b
    result = von_Zett_nach_ZZ $ if b /= 0 then a `div` b else 0


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

