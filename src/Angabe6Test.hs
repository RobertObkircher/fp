module Angabe6Test
    ( module Test.HUnit
    , module Angabe6
    , module Angabe6Test
    )
where

import           Test.HUnit
import           Angabe6

--
-- All tests
--

testAll :: IO ()
testAll = do
    x <- runTestTT testAngabe6
    return ()

testAngabe6 :: Test
testAngabe6 = TestList
    [ testPi0zst0Informell
    , testPi0zst0
    , testPi1
    , testPi2
    , testPi4
    , testPi5
    , testA2
    , testA3ggt
    ]

testNontermination1 = gib_aus_Zustand (interpretiere_1 pi3 zst1)

testNontermination2 = do
    putStr "Expected: "
    print $ map gib_aus_Zustand [zst1]
    putStr "Actual: "
    print $ map gib_aus_Zustand (take 100 (interpretiere_2 pi3 zst1))

--
-- printf debugging is fun
--

showPC :: ProgramCounter -> String
showPC pc = case pc of
    Terminated -> "Terminated"
    (PC left current right index count) ->
        "(PC "
            ++ show index
            ++ " "
            ++ show count
            ++ ":"
            ++ concatMap showA (reverse left)
            ++ '❤'
            :  show current
            ++ "❤"
            ++ concatMap showA right
            ++ ")"
        where showA a = ' ' : show a ++ " "

showState :: Zustand -> String
showState (a, l) =
    "(A"
        ++ concatMap f [A1 .. A6]
        ++ ")"
        ++ "(L"
        ++ concatMap g [L1 .. L6]
        ++ ")"
  where
    f x = " " ++ show (a x)
    g x = " " ++ if l x then "T" else "F"

showExecution :: Execution -> String
showExecution e = showState (state e) ++ " " ++ showPC (pc e)


printExecution :: [Anweisung] -> Anfangszustand -> IO ()
printExecution prog state = do
    let execs = take 100 $ execute (newExecution prog state)
    putStrLn (unlines (map showExecution execs))

--
-- Tests for A1
--

fib n | n == 0    = 0
      | n == 1    = 1
      | n > 0     = fib (n - 2) + fib (n - 1)
      | otherwise = n

fac n | n == 0    = 1
      | n > 0     = n * fac (n - 1)
      | otherwise = n

-- Programm pi0: Berechnung der Fakultaet fuer den Anfangswert von A1, falls
-- dieser groesser oder gleich 0 ist, in A2:
pi0 =
    [ AZ A2 (AK 1)
    , BS (Gleich (AV A1) (AK 1)) 999
    , AZ A2 (Mal (AV A2) (AV A1))
    , AZ A1 (Minus (AV A1) (AK 1))
    , US 1
    ]

lvb0 :: Log_Variablenbelegung
lvb0 lv = True

avb0 :: Arith_Variablenbelegung
avb0 av = if av == A1 then fib 4 else fib 6 - (fac 3 + 2)

zst0 :: Anfangszustand
zst0 = (avb0, lvb0)

avb1 :: Arith_Variablenbelegung
avb1 av = if av == A2 then 1 else avb0 av

avb2 :: Arith_Variablenbelegung
avb2 av = if av == A2 then 3 else avb0 av

avb3 :: Arith_Variablenbelegung
avb3 av = case av of
    A1 -> 2
    A2 -> 3
    x  -> avb0 x

avb4 :: Arith_Variablenbelegung
avb4 av = case av of
    A1 -> 2
    A2 -> 6
    x  -> avb0 x

avb5 :: Arith_Variablenbelegung
avb5 av = case av of
    A1 -> 1
    A2 -> 6
    x  -> avb0 x

-- Informell, keine direkte Ausgabe am Bildschirm moeglich (s.a. A.2).
testPi0zst0Informell :: Test
testPi0zst0Informell = "testPi0zst0Informell" ~: test
    [ gib_aus_Zustand (interpretiere_1 pi0 zst0)
        @?= gib_aus_Zustand (avb5, lvb0)
    , map gib_aus_Zustand (interpretiere_2 pi0 zst0) @?= map
        gib_aus_Zustand
        [ (avb0, lvb0)
        , (avb1, lvb0)
        , (avb2, lvb0)
        , (avb3, lvb0)
        , (avb4, lvb0)
        , (avb5, lvb0)
        ]
    , map gib_aus_Zustand (take 2 (drop 3 (interpretiere_2 pi0 zst0)))
        @?= map gib_aus_Zustand [(avb3, lvb0), (avb4, lvb0)]
    ]


-- Unmittelbare Bildschirmausgaben moeglich z.B. fuer:
testPi0zst0 :: Test
testPi0zst0 = "pi0 zst0" ~: test
    [ fst (interpretiere_1 pi0 zst0) A1 @?= 1
    , fst (interpretiere_1 pi0 zst0) A2 @?= 6
    , fst (interpretiere_1 pi0 zst0) A3 @?= 0
    , fst (interpretiere_1 pi0 zst0) A4 @?= 0
    , fst (interpretiere_1 pi0 zst0) A5 @?= 0
    , fst (interpretiere_1 pi0 zst0) A6 @?= 0
    , fst (avb5, lvb0) A1 @?= 1
    , fst (avb5, lvb0) A2 @?= 6
    , fst (avb5, lvb0) A3 @?= 0
    , fst (avb5, lvb0) A4 @?= 0
    , fst (avb5, lvb0) A5 @?= 0
    , fst (avb5, lvb0) A6 @?= 0
    , avb5 A1 @?= 1
    , avb5 A2 @?= 6
    , avb5 A3 @?= 0
    , avb5 A4 @?= 0
    , avb5 A5 @?= 0
    , avb5 A6 @?= 0
  --
    , fst (interpretiere_2 pi0 zst0 !! 0) A1 @?= 3
    , fst (interpretiere_2 pi0 zst0 !! 1) A6 @?= 0
    , fst (interpretiere_2 pi0 zst0 !! 2) A1 @?= 3
    , fst (interpretiere_2 pi0 zst0 !! 3) A2 @?= 3
    , fst (interpretiere_2 pi0 zst0 !! 4) A1 @?= 2
    , fst (interpretiere_2 pi0 zst0 !! 5) A1 @?= 1
    , fst (interpretiere_2 pi0 zst0 !! 5) A2 @?= 6
    , fst (interpretiere_2 pi0 zst0 !! 5) A3 @?= 0
    , avb0 A1 @?= 3
    , avb1 A6 @?= 0
    , avb2 A1 @?= 3
    , avb3 A2 @?= 3
    , avb4 A1 @?= 2
    , avb5 A1 @?= 1
    , avb5 A2 @?= 6
    , avb5 A3 @?= 0
    ]


-- Programm pi1: Leeres Programm
pi1 = []

testPi1 :: Test
testPi1 = "pi1" ~: test
    [ gib_aus_Zustand (interpretiere_1 pi1 zst0) @?= gib_aus_Zustand zst0
    , map gib_aus_Zustand (interpretiere_2 pi1 zst0)
        @?= map gib_aus_Zustand [zst0]
    ]


-- Programm pi2: Selbstmodifizierendes Programm
pi2 =
    [ MP ((fib (fac 3)) - (fac (1 + 2) + 2))
         (AZ A3 (AK (5 * (fib (fac 3)) + 2)))
    ]

avb6 :: Arith_Variablenbelegung
avb6 av = 1

zst1 :: Anfangszustand
zst1 = (avb6, lvb0)

zst2 :: Anfangszustand
zst2 =
    ( \av -> if av == A3 then fac 3 * (fib 7 - fac 3) else avb6 av
    , \lv -> (lv /= L6) || (mod (4 * avb6 A3) (2 + fac (avb6 A1)) /= 0)
    )


testPi2 :: Test
testPi2 = "pi2" ~: test
    [ gib_aus_Zustand (interpretiere_1 pi2 zst1) @?= gib_aus_Zustand
        (\av -> if av == A3 then 42 else fst zst1 av, snd zst1)
    , map gib_aus_Zustand (interpretiere_2 pi2 zst1) @?= map
        gib_aus_Zustand
        [zst1, (\av -> if av == A3 then 42 else fst zst1 av, snd zst1)]
    , gib_aus_Zustand (interpretiere_1 pi2 zst2)
        @?= gib_aus_Zustand (interpretiere_1 pi2 zst1)
    --, map gib_aus_Zustand (interpretiere_2 pi2 zst2) @?= map gib_aus_Zustand (interpretiere_2 pi2 zst1) --Der erste Zustand in dieser Liste (d.h. an Indexposition 0) ist der Anfangszustand σ
    ]


-- Programm pi3: Selbstmodifizierendes, nichtterminierendes Programm
pi3 =
    [ MP ((fac 3 + 2) - (fib (fac 3)))
         (BS (LK ((fac 5) > (fib 5))) (fib (fac 3) - (fac 3 + 2)))
        ]
        ++ pi0
        ++ pi2

{-
testPi3 :: Test
testPi3 = "testPi3" ~: test
    [ gib_aus_Zustand (interpretiere_1 pi3 zst1) @?= error "Should not terminate"
    , map gib_aus_Zustand (take 100 (interpretiere_2 pi3 zst1)) @?= map gib_aus_Zustand [zst1]
    ]
    -}

{-
drop 2 (interpretiere_2 pi3 zst1) @?= ...
-- unendliches Warten, da interpretiere_2 nicht terminiert,
-- aber nach dem ersten keine weiteren Listenelemente liefert.
-}

-- Programm pi4: Nichtterminierendes, kontinuierlich zustands-
-- veraenderndes Programm
pi4 = [LZ L1 (Nicht (LV L1)), BS (LK True) 0]
--interpretiere_1 pi4 zst1 @?= ‘undefiniert wg. Nichtterminierung’
--interpretiere_2 pi4 zst1 @?= [zst1,zst2,zst1,zst2,zst1,zst2,..
-- terminiert nicht regulaer, sondern irregulaer, wenn der gesamte
-- Maschinenspeicher aufgebraucht ist
--    wobei zst2 = (fst zst1,\ lv -> if lv == L1 then False else True)

testPi4 :: Test
testPi4 = "testPi4" ~: test
    [ -- map gib_aus_Zustand (take 4 (interpretiere_2 pi4 zst1)) @?= map gib_aus_Zustand [zst1, zst2, zst1, zst2] -- ????????????
      fst (take 4 (interpretiere_2 pi4 zst1) !! 3) A3 @?= 1
    , snd (take 4 (interpretiere_2 pi4 zst1) !! 1) L1 @?= False
    , snd (take 4 (interpretiere_2 pi4 zst1) !! 1) L2 @?= True
    ]

-- Programm pi5: Nichtterminierendes, formal (nicht tatsaechlich) zustands-
-- veraenderndes Programm (der Zustand wird ident ueberschrieben)
pi5 = [LZ L1 (LV L1), BS (LK True) 0]
--interpretiere_1 pi5 zst1 @?= ‘undefiniert wg. Nichtterminierung’
--interpretiere_2 pi5 zst1 @?= [zst1,zst1,zst1,zst1,zst1,..
-- terminiert nicht regulaer, sondern irregulaer, wenn der gesamte
-- Maschinenspeicher aufgebraucht ist
--    wobei zst2 = (fst zst1,\ lv -> if lv == L1 then False else True)

testPi5 :: Test
testPi5 = "testPi5" ~: test
    [ map gib_aus_Zustand (take 5 (interpretiere_2 pi5 zst1))
        @?= map gib_aus_Zustand [zst1, zst1, zst1, zst1, zst1]
    , fst (take 5 (interpretiere_2 pi5 zst1) !! 3) A3 @?= 1
    , snd (take 4 (interpretiere_2 pi5 zst1) !! 1) L1 @?= True
    , snd (take 4 (interpretiere_2 pi5 zst1) !! 1) L2 @?= True
    , snd (take 5 (interpretiere_2 pi5 zst1) !! 2) L5 @?= True
    ]


avb :: Arith_Variablenbelegung
avb av = 42

lvb :: Log_Variablenbelegung
lvb lv = True

zst :: Zustand
zst = (avb, lvb)

testA2 :: Test
testA2 = "testA2" ~: test
    [ gib_aus_arith_Varbel avb
        @?= [(A1, 42), (A2, 42), (A3, 42), (A4, 42), (A5, 42), (A6, 42)]
    , gib_aus_log_Varbel lvb
        @?= [ (L1, True)
            , (L2, True)
            , (L3, True)
            , (L4, True)
            , (L5, True)
            , (L6, True)
            ]
    , gib_aus_Zustand zst
        @?= ( [(A1, 42), (A2, 42), (A3, 42), (A4, 42), (A5, 42), (A6, 42)]
            , [ (L1, True)
              , (L2, True)
              , (L3, True)
              , (L4, True)
              , (L5, True)
              , (L6, True)
              ]
            )
    , gib_aus_Zustand (interpretiere_1 pi2 zst1)
        @?= ( [(A1, 1), (A2, 1), (A3, 42), (A4, 1), (A5, 1), (A6, 1)]
            , [ (L1, True)
              , (L2, True)
              , (L3, True)
              , (L4, True)
              , (L5, True)
              , (L6, True)
              ]
            )
    ]

testA3ggt :: Test
testA3ggt = "testA3ggt" ~: test
    [ fst
              (interpretiere_1
                  ggt
                  ( \a ->
                      if a == A1 then x else (if a == A2 then y else undefined)
                  , const True
                  )
              )
              A3
          @?= gcd x y
    | x <- [1 .. 20]
    , y <- [1 .. 20]
    ]


testA3Fibo :: Test
testA3Fibo = "testA3Fibo" ~: test
    [ a6l1 (interpretiere_1 fibo (init n)) @?= (fibo' n, n >= 0)
    | n <- [-20 .. 20]
    ]
  where
    a6l1 (a, l) = (a A6, l L1)
    init n = (\a -> if a == A1 then n else undefined, const undefined)
    fibo' n | n >= 0    = fib n
            | otherwise = -fib n


testA3c :: Test
testA3c = "testA3c" ~: test
    [ gib_aus_Zustand azst1
        @?= ( [(A1, 24), (A2, 60), (A3, 0), (A4, 0), (A5, 0), (A6, 0)]
            , [ (L1, True)
              , (L2, True)
              , (L3, True)
              , (L4, True)
              , (L5, True)
              , (L6, True)
              ]
            )
    , gib_aus_Zustand azst2
        @?= ( [(A1, 18), (A2, 45), (A3, 3), (A4, 4), (A5, 5), (A6, 6)]
            , [ (L1, False)
              , (L2, True)
              , (L3, False)
              , (L4, True)
              , (L5, False)
              , (L6, True)
              ]
            )
    , gib_aus_Zustand (generiere [] [])
        @?= ( [(A1, 0), (A2, 0), (A3, 0), (A4, 0), (A5, 0), (A6, 0)]
            , [ (L1, False)
              , (L2, False)
              , (L3, False)
              , (L4, False)
              , (L5, False)
              , (L6, False)
              ]
            )
    , gib_aus_Zustand (generiere [4, 5] [True, True])
        @?= ( [(A1, 4), (A2, 5), (A3, 0), (A4, 0), (A5, 0), (A6, 0)]
            , [ (L1, True)
              , (L2, True)
              , (L3, False)
              , (L4, False)
              , (L5, False)
              , (L6, False)
              ]
            )
    ]
