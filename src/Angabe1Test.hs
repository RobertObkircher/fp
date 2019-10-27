module Angabe1Test
    ( module Test
    , module Angabe1
    , module Angabe1Test
    ) where

import Test
import Angabe1

tc_streiche :: [TestCase]
tc_streiche = 
    [ streiche "abcabcabcabcabc" 2 'b'    ->> "abcacabcacabc"
    , streiche "abcabcabcabcabc" 3 'b'    ->> "abcabcacabcabc"
    , streiche "abcabcabcabcabc" 0 'b'    ->> "abcabcabcabcabc"
    , streiche "abcabcabcabcabc" (-5) 'b' ->> "abcabcabcabcabc"
    , streiche "abcabcabcabcabc" 2 'd'    ->> "abcabcabcabcabc"
-- custom
    , streiche "" 3 ' '                   ->> ""
    , streiche "aaa" 1 'a'                ->> ""
    ]

tc_ist_umgekehrt_2er_potenz :: [TestCase]
tc_ist_umgekehrt_2er_potenz =
    [ ist_umgekehrt_2er_potenz 61 ->> True
    , ist_umgekehrt_2er_potenz 16 ->> False
    , ist_umgekehrt_2er_potenz 46 ->> True
    , ist_umgekehrt_2er_potenz 64 ->> False
    , ist_umgekehrt_2er_potenz 1 ->> True
    , ist_umgekehrt_2er_potenz 0 ->> False
    ]

tc_groesstes_palindrom_in =
  [ groesstes_palindrom_in [11,1,5,3,7,1221,11,2,1221,5] ->> 1221
  , groesstes_palindrom_in [112,1,5,3,7,0,4,113,2,5,0] ->> 7
  , groesstes_palindrom_in [12,13,14] ->> (-1)
-- custom
  , groesstes_palindrom_in [] ->> (-1)
  , groesstes_palindrom_in [213094] ->> (-1)
  ]


