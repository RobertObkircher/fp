module Angabe7Test
    ( module Test.HUnit
    , module Angabe7
    , module Angabe7Test
    )
where

import           Test.HUnit
import           Angabe7

--
-- All tests
--

testAll :: IO ()
testAll = do
    x <- runTestTT testAngabe7
    return ()

testAngabe7 :: Test
testAngabe7 = TestList [testA1, testA2, testA3, testA4]

testA1 :: Test
testA1 = "testA1" ~: test
    [ take 9 generiere_fak_strom @?= [1, 1, 2, 6, 24, 120, 720, 5040, 40320]
    , take 5 generiere_fak_strom @?= [1, 1, 2, 6, 24]
    , take 4 (filter (> 100) generiere_fak_strom) @?= [120, 720, 5040, 40320]
    , take 7 (filter (< 10000) generiere_fak_strom)
        @?= [1, 1, 2, 6, 24, 720, 5040]
    ]

testA2 :: Test
testA2 = "testA2" ~: test
    [ take 3 (generiere_exp_strom 1.0) @?= [1.0, 2.0, 2.5]
    , exp_approx 1.0 0.5 @?= 2.5
    , selektiere 0.5 (generiere_exp_strom 1.0) @?= 2.5
    , exp_approx 1.0 0.2 @?= 2 * 2 / 3
    , selektiere 0.2 (generiere_exp_strom 1.0) @?= 2 * 2 / 3
    ]

testA3 :: Test
testA3 = "testA3" ~: test
    [ take 20 generiere_woerter
        @?= [ ""
            , "a"
            , "b"
            , "c"
            , "aa"
            , "ab"
            , "ac"
            , "ba"
            , "bb"
            , "bc"
            , "ca"
            , "cb"
            , "cc"
            , "aaa"
            , "aab"
            , "aac"
            , "aba"
            , "abb"
            , "abc"
            , "aca"
            , "acb"
            ]
    , take 10 (filtere_palindrome generiere_woerter)
        @?= ["", "a", "b", "c", "aa", "bb", "cc", "aaa", "aba", "aca"]
    , [ s | s <- filtere_palindrome generiere_woerter, length s == 2 ]
        @?= ["aa", "bb", "cc"]
    ]

woerterbuch =
    [ "awake"
    , "awaken"
    , "cat"
    , "dig"
    , "dog"
    , "fig"
    , "fin"
    , "fine"
    , "fog"
    , "log"
    , "rake"
    , "wake"
    , "wine"
    ]

testA4 :: Test
testA4 = "testA4" ~: test
    [ gib_max_aufsteigende_wortleiter woerterbuch
        @?= ["dig", "fig", "fin", "fine", "wine"] -- Leiterlaenge 5
    , length (gib_max_aufsteigende_wortleiter woerterbuch) @?= 5
    , ist_aufsteigende_leiterstufe "fig" "fin" @?= True
    , ist_aufsteigende_leiterstufe "fin" "fig" @?= False
    , ist_aufsteigende_wortleiter woerterbuch @?= False
    , ist_aufsteigende_wortleiter ["dig", "fig", "fin"] @?= True
    , ist_aufsteigende_wortleiter ["fin", "fig", "dig"] @?= False
    ]
