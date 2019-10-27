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


x :: Integer
x = von_ZZ_nach_Zett $ plus (von_Zett_nach_ZZ 900000) (von_Zett_nach_ZZ 123456789)

main = print x
