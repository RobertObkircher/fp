module Test where

data TestCase = TestCase String String Bool

(->>) :: (Eq a, Show a) => a -> a -> TestCase
result ->> expected = TestCase (show result) (show expected) (result == expected)

infixl 0 ->>

printTestCase :: TestCase -> String
printTestCase (TestCase result expected success) = msg success ++ result ++ " ->> " ++ expected
  where
    msg :: Bool -> String
    msg True = "OK: "
    msg False = "FAIL: "

test :: [TestCase] -> IO ()
test = putStrLn . unlines . map printTestCase

