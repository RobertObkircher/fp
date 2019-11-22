module Angabe6Test
    ( module Test
    , module Angabe6
    , module Angabe6Test
    ) where

import Test
import Angabe6

test_all = do
    test

--
--
--

fib n
  | n == 0 = 0
  | n == 1 = 1
  | n > 0 = fib (n-2) + fib (n-1)
  | True = n

fac n
  | n == 0 = 1
  | n > 0 = n * fac (n-1)
  | True = n

-- Programm pi0: Berechnung der Fakultaet fuer den Anfangswert von A1, falls
-- dieser groesser oder gleich 0 ist, in A2:
pi0 = [AZ A2 (AK 1),BS (Gleich (AV A1) (AK 1)) 999, AZ A2 (Mal (AV A2) (AV A1)),AZ A1 (Minus (AV A1) (AK 1)),US 1]


lvb0 :: Log_Variablenbelegung
lvb0 lv =  True

avb0 :: Arith_Variablenbelegung
avb0 av =  if av == A1 then fib 4 else fib 6 - (fac 3 + 2)

zst0 :: Anfangszustand
zst0 = (avb0,lvb0)

avb1 :: Arith_Variablenbelegung
avb1 av = if av == A2 then 1 else avb0 av

avb2 :: Arith_Variablenbelegung
avb2 av = if av == A2 then 3 else avb0 av

avb3 :: Arith_Variablenbelegung
avb3 av = case av of
  A1 -> 2
  A2 -> 3
  x -> avb0 x

avb4 :: Arith_Variablenbelegung
avb4 av = case av of
  A1 -> 2
  A2 -> 6
  x -> avb0 x

avb5 :: Arith_Variablenbelegung
avb5 av = case av of
  A1 -> 1
  A2 -> 6
  x -> avb0 x

{-

interpretiere_1 pi0 zst0 ->> (avb5,lvb0) -- Informell, keine direkte Ausgabe am Bildschirm moeglich (s.a. A.2).
interpretiere_2 pi0 zst0 ->> [(avb0,lvb0),(avb1,lvb0),(avb2,lvb0), (avb3,lvb0),(avb4,lvb0),(avb5,lvb0)] -- s.o.
take 2 (drop 3 (interpretiere_2 pi0 zst0)) ->> [(avb3,lvb0),(avb4,lvb0)] -- s.o.

-- Unmittelbare Bildschirmausgaben moeglich z.B. fuer:
fst (interpretiere_1 pi0 zst0) A1 ->> fst (avb5,lv0) A1 ->> avb5 A1 ->> 1
fst (interpretiere_1 pi0 zst0) A2 ->> fst (avb5,lv0) A2 ->> avb5 A2 ->> 6
fst (interpretiere_1 pi0 zst0) A3 ->> fst (avb5,lv0) A3 ->> avb5 A3 ->> 0
fst (interpretiere_1 pi0 zst0) A4 ->> fst (avb5,lv0) A4 ->> avb5 A4 ->> 0
fst (interpretiere_1 pi0 zst0) A5 ->> fst (avb5,lv0) A5 ->> avb5 A5 ->> 0
fst (interpretiere_1 pi0 zst0) A6 ->> fst (avb5,lv0) A6 ->> avb5 A6 ->> 0

fst ((interpretiere_2 pi0 zst0) !!  0) A1 ->> ...  ->> avb0 A1 ->> 3
fst ((interpretiere_2 pi0 zst0) !!  1) A6 ->> ...  ->> avb1 A6 ->> 0
fst ((interpretiere_2 pi0 zst0) !!  2) A1 ->> ...  ->> avb2 A1 ->> 3
fst ((interpretiere_2 pi0 zst0) !!  3) A2 ->> ...  ->> avb3 A2 ->> 3
fst ((interpretiere_2 pi0 zst0) !!  4) A1 ->> ...  ->> avb4 A1 ->> 2
fst ((interpretiere_2 pi0 zst0) !!  5) A1 ->> ...  ->> avb5 A1 ->> 1
fst ((interpretiere_2 pi0 zst0) !!  5) A2 ->> ...  ->> avb5 A2 ->> 6
fst ((interpretiere_2 pi0 zst0) !!  5) A3 ->> ...  ->> avb5 A3 ->> 0

-- Programm pi1: Leeres Programm
pi1 = []
interpretiere_1 pi1 zst0 ->> zst0 -- s.o.
interpretiere_2 pi1 zst0 ->> [zst0] -- s.o.

-- Programm pi2: Selbstmodifizierendes Programm
pi2 = [MP ((fib (fac 3)) - (fac (1+2) + 2)) (AZ A3 (AK (5*(fib (fac 3))+2)))]

avb6 = \ av -> 1 :: Arith_Variablenbelegung
zst1 = (avb6,lvb0) :: Anfangszustand
zst2 = (\ av -> if av == A3 then (fac 3) * (fib 7 - fac 3) else avb6 av,
        \ lv -> if lv /= L6
                    then (True /= False) && True
                    else (mod (4 * (avb6 A3)) (2 + fac (avb6 A1)) /= 0) )
       :: Anfangszustand

interpretiere_1 pi2 zst1 ->> (\ av -> if av == A3 then 42 else fst zst1 av,snd zst1) -- s.o.

interpretiere_2 pi2 zst1 ->> [zst1,(\ av -> if av == A3 then 42 else fst zst1 av,snd zst1)] -- s.o.
interpretiere_1 pi2 zst2 ‘==’ interpretiere_1 pi2 zst1 -- symbolisch: (Listen
interpretiere_2 pi2 zst2 ‘==’ interpretiere_2 pi2 zst1 -- von) Fkt. nicht in Eq


-- Programm pi3: Selbstmodifizierendes, nichtterminierendes Programm
pi3 = [MP ((fac 3 + 2) - (fib (fac 3)))
            (BS (LK ((fac 5) > (fib 5))) (fib (fac 3) - (fac 3 + 2)))]
        ++ pi0 ++ pi2

interpretiere_1 pi3 zst1 ->> ‘undefiniert wg. Nichtterminierung’
interpretiere_2 pi3 zst1 ->> [zst1.. -- terminiert nicht, aber fuehrt keine
                                     -- zustandsveraendernde Anweisung aus;
                                     -- deshalb erscheint nur das erste Listen-
                                     -- element gefolgt von unendlichem Warten.

drop 2 (interpretiere_2 pi3 zst1) ->> ...
-- unendliches Warten, da interpretiere_2 nicht terminiert,
-- aber nach dem ersten keine weiteren Listenelemente liefert.

-- Programm pi4: Nichtterminierendes, kontinuierlich zustands-
-- veraenderndes Programm
pi4 = [LZ L1 (Nicht (LV L1)), BS (LK True) 0]
interpretiere_1 pi4 zst1 ->> ‘undefiniert wg. Nichtterminierung’
interpretiere_2 pi4 zst1 ->> [zst1,zst2,zst1,zst2,zst1,zst2,..
-- terminiert nicht regulaer, sondern irregulaer, wenn der gesamte
-- Maschinenspeicher aufgebraucht ist
    wobei zst2 = (fst zst1,\ lv -> if lv == L1 then False else True)

take 4 (interpretiere_2 pi4 zst1) ->> [zst1,zst2,zst1,zst2] -- nicht unmittelbar ausgebbar, s.o.
fst (take 4 (interpretiere_2 pi4 zst1) !! 3) A3 ->> ... ->> 1
snd (take 4 (interpretiere_2 pi4 zst1) !! 1) L1 ->> ... ->> False
snd (take 4 (interpretiere_2 pi4 zst1) !! 1) L2 ->> ... ->> True

-- Programm pi5: Nichtterminierendes, formal (nicht tatsaechlich) zustands-
-- veraenderndes Programm (der Zustand wird ident ueberschrieben)
pi5 = [LZ L1 (LV L1), BS (LK True) 0]
interpretiere_1 pi5 zst1 ->> ‘undefiniert wg. Nichtterminierung’
interpretiere_2 pi5 zst1 ->> [zst1,zst1,zst1,zst1,zst1,..
-- terminiert nicht regulaer, sondern irregulaer, wenn der gesamte
-- Maschinenspeicher aufgebraucht ist
    wobei zst2 = (fst zst1,\ lv -> if lv == L1 then False else True)

take 5 (interpretiere_2 pi5 zst1) ->> [zst1,zst1,zst1,zst1,zst1] -- nicht unmittelbar ausgebbar, s.o.
fst (take 5 (interpretiere_2 pi5 zst1) !! 3) A3 ->> ... ->> 1
snd (take 4 (interpretiere_2 pi5 zst1) !! 1) L1 ->> ... ->> True
snd (take 4 (interpretiere_2 pi5 zst1) !! 1) L2 ->> ... ->> True
snd (take 5 (interpretiere_2 pi5 zst1) !! 2) L5 ->> ... ->> True

-}
