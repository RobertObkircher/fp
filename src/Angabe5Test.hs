module Angabe5Test
    ( module Test
    , module Angabe5
    , module Angabe5Test
    ) where

import Test
import Angabe5

test_all = do
    test tc_bsp1
    test tc_bsp2
    test tc_bsp3
    test tc_a2

tageAnzahl :: Wochentag -> [Nat0] -> [Tag]
tageAnzahl first = zipWith Tag $ drop (fromEnum first) (cycle woche)

woche :: [Wochentag]
woche = map toEnum [0..6]

bsp1 :: Modellszenario
bsp1 = (Mo, 15, map P [2..5])

bsp1Anzahl :: [Nat0]
bsp1Anzahl = [0,1,1,2,0,2,0,2,1,2,0,0,0,0,2]

tc_bsp1 :: [TestCase]
tc_bsp1 =
    [ tage bsp1 ->> tageAnzahl Mo bsp1Anzahl
    , streiksProTag bsp1 ->> bsp1Anzahl
    , streiktage bsp1 ->> 8
    , superstreiktage bsp1 ->> 0
    , grossstreiktage bsp1 0 ->> 15
    , grossstreiktage bsp1 1 ->> 8
    , grossstreiktage bsp1 2 ->> 5
    , grossstreiktage bsp1 3 ->> 0
    , grossstreiktage bsp1 4 ->> 0
    , grossstreiktage bsp1 5 ->> 0
    , streiktage_am bsp1 Mi 0 ->> 2
    , streiktage_am bsp1 Mi 1 ->> 2
    , streiktage_am bsp1 Mi 2 ->> 1
    , streiktage_am bsp1 Mi 3 ->> 0
    , wird_gestreikt bsp1 1 ->> False
    , wird_gestreikt bsp1 2 ->> True
    , wird_gestreikt bsp1 7 ->> False
    , wird_gestreikt bsp1 10 ->> True
    ]

bsp2 :: Modellszenario
bsp2 = (Mi, 15, map P [2..5])

bsp2Anzahl :: [Nat0]
bsp2Anzahl = [0,1,0,2,0,2,0,2,1,0,0,0,0,1,2]

tc_bsp2 :: [TestCase]
tc_bsp2 =
    [ tage bsp2 ->> tageAnzahl Mi bsp2Anzahl
    , streiksProTag bsp2 ->> bsp2Anzahl
    , streiktage bsp2 ->> 7
    , superstreiktage bsp2 ->> 0
    , grossstreiktage bsp2 0 ->> 15
    , grossstreiktage bsp2 1 ->> 7
    , grossstreiktage bsp2 2 ->> 4
    , grossstreiktage bsp2 3 ->> 0
    , grossstreiktage bsp2 4 ->> 0
    , grossstreiktage bsp2 5 ->> 0
    , streiktage_am bsp2 Mi 0 ->> 3
    , streiktage_am bsp2 Mi 1 ->> 2
    , streiktage_am bsp2 Mi 2 ->> 2
    , streiktage_am bsp2 Mi 3 ->> 0
    , wird_gestreikt bsp2 1 ->> False
    , wird_gestreikt bsp2 2 ->> True
    , wird_gestreikt bsp2 3 ->> False
    , wird_gestreikt bsp2 4 ->> True
    ]

bsp3 :: Modellszenario
bsp3 = (Do, 13, map P [2..3])

bsp3Anzahl :: [Nat0]
bsp3Anzahl = [0,0,1,0,0,2,0,1,0,1,0,2,0]

tc_bsp3 :: [TestCase]
tc_bsp3 =
    [ tage bsp3 ->> tageAnzahl Do bsp3Anzahl
    , streiksProTag bsp3 ->> bsp3Anzahl
    , streiktage bsp3 ->> 5
    , superstreiktage bsp3 ->> 2
    , grossstreiktage bsp3 0 ->> 13
    , grossstreiktage bsp3 1 ->> 5
    , grossstreiktage bsp3 2 ->> 2
    , grossstreiktage bsp3 3 ->> 0
    , grossstreiktage bsp3 4 ->> 0
    , grossstreiktage bsp3 5 ->> 0
    , streiktage_am bsp3 Di 0 ->> 2
    , streiktage_am bsp3 Di 1 ->> 1
    , streiktage_am bsp3 Di 2 ->> 1
    , streiktage_am bsp3 Di 3 ->> 0
    , wird_gestreikt bsp3 1 ->> False
    , wird_gestreikt bsp3 2 ->> False
    , wird_gestreikt bsp3 3 ->> True
    , wird_gestreikt bsp3 4 ->> False
    , wird_gestreikt bsp3 6 ->> True
    ]


avb = const 3 :: Arith_Variablenbelegung
lvb = const True :: Log_Variablenbelegung
vb1 = (avb,lvb) :: Variablenbelegung
vb2 = (\ av -> if av == A1 then 42 else avb av,
       \ lv -> if lv /= L6 then False else (mod (avb A3) 3 == 0)
       ) :: Variablenbelegung

aa1 = Mal (Plus (AV A1) (AV A2)) (Mal (AV A1) (AV A2))
aa2 = AK 42
la1 = Nicht (Kleiner aa1 aa1)
la2 = Oder (Nicht (Gleich aa1 aa1)) (Oder (Nicht (LV L3)) (Nicht la1))


tc_a2 =
    [ links (evaluiere aa1 vb1) ->> links (Left 54)
    , links (evaluiere aa1 vb2) ->> links (Left 5670)
    , links (evaluiere aa2 vb1) ->> links (Left 42)
    , links (evaluiere aa2 vb2) ->> links (Left 42)
    , rechts (evaluiere la1 vb1) ->> rechts (Right True)
    , rechts (evaluiere la1 vb2) ->> rechts (Right True)
    , rechts (evaluiere la2 vb1) ->> rechts (Right False)
    , rechts (evaluiere la2 vb2) ->> rechts (Right True)
    , links (evaluiere (Minus aa2 (AK 3)) vb1) ->> 39
    ]

