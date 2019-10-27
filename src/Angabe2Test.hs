module Angabe2Test
    ( module Test
    , module Angabe2
    , module Angabe2Test
    ) where

import Test
import Angabe2

tc_dp :: [TestCase]
tc_dp =
    [ dp (4,20) ->> [5,7,11,13,17]
    , dp (12,32) ->> [13,17,31]
    , dp (32,12) ->> [13,17,31]
    , dp (15,15) ->> []
    ]

tc_folge :: [TestCase]
tc_folge =
    [ folge 3 ->> [3,1]
    , folge 4 ->> [4,3,1]
    , folge 6 ->> [6] --(Zyklus der Laenge 1)
    , folge 10 ->> [10,8,7,1]
    , folge 12 ->> [12,16,15,9,4,3,1]
    , folge 220 ->> [220,284] --(Zyklus der Laenge 2)
    , folge 284 ->> [284,220] --(Zyklus der Laenge 2)
    , folge 12496 ->> [12496,14288,15472,14536,14264] --(Zyklus der Laenge 4)
    , length (folge 14316) ->> 28 
    ]


tc_medianoid :: [TestCase]
tc_medianoid =
    [ medianoid [2,5,7,9,11] ->>7
    , medianoid [5,2,9,7,11] ->>7
    , medianoid [2,5,7,9,11,13] ->>9
    , medianoid [2,5,7,9,11,13,17] ->>9
-- custom
    , medianoid [] ->> sum []
    , medianoid [1,1,2,3,4] ->> sum [1,1,2,3,4]
    , medianoid [1,2,2,3,4] ->> sum [1,2,2,3,4]
    , medianoid [1,2,3,4,4] ->> sum [1,2,3,4,4]
    ]
 
