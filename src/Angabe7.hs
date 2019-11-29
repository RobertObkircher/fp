module Angabe7 where

import           Control.Monad
import           Data.Ord                       ( Down(..)
                                                , comparing
                                                )
import           Data.List                      ( sortOn
                                                , maximumBy
                                                )

--
-- A1
--

generiere_fak_strom :: [Integer]
generiere_fak_strom = 1 : zipWith (*) generiere_fak_strom [1 ..]

--
-- A2
--

type IR_plus = Double -- Nur Werte echt groesser als null
type Stelle = Double
type Genauigkeit = IR_plus
type Approx_Wert = Double
type Strom = [Double]


approximiere_exp :: Stelle -> Genauigkeit -> Approx_Wert
approximiere_exp x epsilon = selektiere epsilon (generiere_exp_strom x)

exp_approx :: Stelle -> Genauigkeit -> Approx_Wert
exp_approx = approximiere_exp

generiere_exp_strom :: Stelle -> Strom
generiere_exp_strom x = tail $ scanl (+) 0 $ zipWith
    (/)
    (iterate (* x) 1)
    (map fromInteger generiere_fak_strom)

selektiere :: Genauigkeit -> Strom -> Approx_Wert
selektiere epsilon s = snd $ head $ dropWhile f $ zip s (tail s)
  where
    f :: (Double, Double) -> Bool
    f (a, b) = abs (a - b) > epsilon

--
-- A3
--

type Woerterstrom = [String]

generiere_woerter :: Woerterstrom
--generiere_woerter = "" : concatMap (\x -> [x ++ "a", x++"b", x++"c"]) generiere_woerter
--generiere_woerter = "" : concatMap (\x -> map ((x++) . (:[])) "abc") generiere_woerter
--generiere_woerter = "" : [ w ++ [a] | w <- generiere_woerter, a <- "abc" ]
--generiere_woerter = concatMap (\n -> replicateM n "abc") [1..]
generiere_woerter = concatMap (`replicateM` "abc") [0 ..]

{-
generiere_woerter = "" : do
  w <- generiere_woerter
  a <- "abc"
  pure (w ++ [a])
-}
filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = filter (\x -> x == reverse x)

--
-- A4
--

type Wort = String
type Woerterbuch = [Wort] -- nur Werte endliche Laenge; keine Stroeme.
type Wortleiter = [Wort]

ist_aufsteigende_leiterstufe :: Wort -> Wort -> Bool
ist_aufsteigende_leiterstufe a@(_ : withoutFirst) b =
    b > a && (withoutFirst == b || wortleiter a b)
ist_aufsteigende_leiterstufe a b = b > a && wortleiter a b

ist_aufsteigende_wortleiter :: [Wort] -> Bool
ist_aufsteigende_wortleiter (x : y : ys) =
    ist_aufsteigende_leiterstufe x y && ist_aufsteigende_wortleiter (y : ys)
ist_aufsteigende_wortleiter _ = True

gib_max_aufsteigende_wortleiter :: Woerterbuch -> Wortleiter
gib_max_aufsteigende_wortleiter = reconstruct . newDp

--
--

wortleiter :: Wort -> Wort -> Bool
wortleiter [] []  = True
wortleiter [] [_] = True
wortleiter (x : xs) (y : ys) | x == y = wortleiter xs ys
                             | x /= y = xs == ys
wortleiter _ _ = False

data Dp = Dp
  { index :: Int
  , wort :: Wort
  , value :: Int
  } deriving (Show)

newDp :: Woerterbuch -> [Dp]
newDp []          = []
newDp woerterbuch = dp
  where
    dp :: [Dp]
    dp = zipWith f [0 ..] $ sortOn Down woerterbuch
    f :: Int -> Wort -> Dp
    f 0 w = Dp 0 w 1
    f n w = Dp n w $ maximum $ map val $ take n dp
      where
        val greater = if ist_aufsteigende_leiterstufe w (wort greater)
            then value greater + 1
            else 1


reconstruct :: [Dp] -> Wortleiter
reconstruct = reverse . map wort . foldl f [] . dropUntilBest . reverse
  where
    f :: [Dp] -> Dp -> [Dp]
    f []       d = [d]
    f (x : xs) d = if match x d then d : x : xs else x : xs
    match x d =
        value d == value x - 1 && ist_aufsteigende_leiterstufe (wort x) (wort d)

dropUntilBest :: [Dp] -> [Dp]
dropUntilBest dp = dropWhile notBest dp
  where
    best :: Dp
    best = maximumBy (comparing value) dp
    notBest :: Dp -> Bool
    notBest d = value d /= value best
