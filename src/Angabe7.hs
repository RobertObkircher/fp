module Angabe7 where

import           Control.Monad
import           Data.Ord                       ( comparing )
import           Data.List                      ( sort
                                                , maximumBy
                                                )

--
-- A1
--

-- the next factorial can be computed by multiplying its index with the last factorial
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

generiere_exp_strom :: Stelle -> Strom
generiere_exp_strom x = scanl1 (+)
    $ zipWith (/) (iterate (* x) 1) (map fromInteger generiere_fak_strom)

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
--generiere_woerter = "" : [ w ++ [a] | w <- generiere_woerter, a <- "abc" ]
{-
generiere_woerter = "" : do
  w <- generiere_woerter
  a <- "abc"
  pure (w ++ [a])
-}
--generiere_woerter = "" : concatMap (\x -> [x ++ "a", x++"b", x++"c"]) generiere_woerter
--generiere_woerter = concatMap (\n -> replicateM n "abc") [1..]
generiere_woerter = concatMap (`replicateM` "abc") [0 ..]

filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = filter (\x -> x == reverse x)

--
-- A4
--

type Wort = String
type Woerterbuch = [Wort] -- nur Werte endliche Laenge; keine Stroeme.
type Wortleiter = [Wort]

ist_aufsteigende_leiterstufe :: Wort -> Wort -> Bool
ist_aufsteigende_leiterstufe a b = case a of
    (_ : as) -> b > a && (as == b || go a b)
    _        -> b > a && go a b
  where
    go :: Wort -> Wort -> Bool
    go [] []  = True
    go [] [_] = True
    go (x : xs) (y : ys) | x == y = go xs ys
                         | x /= y = xs == ys
    go _ _ = False

ist_aufsteigende_wortleiter :: [Wort] -> Bool
ist_aufsteigende_wortleiter (x : y : ys) =
    ist_aufsteigende_leiterstufe x y && ist_aufsteigende_wortleiter (y : ys)
ist_aufsteigende_wortleiter _ = True

gib_max_aufsteigende_wortleiter :: Woerterbuch -> Wortleiter
gib_max_aufsteigende_wortleiter = reconstruct . newDp
-- O(2^n)
--gib_max_aufsteigende_wortleiter = maximumBy (comparing length) . filter ist_aufsteigende_wortleiter . filterM (const [True, False]) . sort

-- From the definition of ist_aufsteigende_leiterstufe follows,
-- that ist_aufsteigende_wortleiter can only be True if the
-- original words are sorted. This means that we can use dynamic
-- programming where the value is the longest aufsteigende_wortleiter
-- that only consists of the words that are lexographically larger.
--
-- Descending order
-- f(0) = 1, since there are no larger elements
-- f(n) = 1 + max({0} U { f(x) | x < n and aufsteigende_wortleiter(x,n) })
--
data Dp = Dp
  { wort :: Wort
  , value :: Int
  } deriving (Show)


newDp :: Woerterbuch -> [Dp]
newDp []          = []
newDp woerterbuch = dp
  where
    dp :: [Dp]
    dp = zipWith f [0 ..] $ reverse $ sort woerterbuch -- sorry hlint, we can't use sortOn Down because of hugs...
    f :: Int -> Wort -> Dp
    f n w = Dp w $ 1 + maximum (0 : map value (filter matching $ take n dp))
      where
        matching :: Dp -> Bool
        matching = ist_aufsteigende_leiterstufe w . wort


reconstruct :: [Dp] -> Wortleiter
reconstruct [] = []
reconstruct dp = reverse $ map wort $ foldr f [best] dp
  where
    f :: Dp -> [Dp] -> [Dp]
    f d xs@(x : _) = if match x d then d : xs else xs
    match :: Dp -> Dp -> Bool
    match s b =
        value b == value s - 1 && ist_aufsteigende_leiterstufe (wort s) (wort b)
    best :: Dp
    best = maximumBy (comparing value) dp
