> module Angabe4 where

> data IN_1 = Eins | Nf IN_1
> data ZZ = Null | Plus IN_1 | Minus IN_1

> type Zett = Integer

A.1

> instance Eq ZZ where
>     (==) a b = a - b == Null
> 
> instance Ord ZZ where
>     compare = compareZZ
> 
> instance Enum ZZ where
>     toEnum = von_Zett_nach_ZZ . toInteger
>     fromEnum = fromEnum . von_ZZ_nach_Zett
>     succ = inc
>     pred = dec
> 
> instance Num ZZ where
>     (+) = plus
>     (-) = minus
>     (*) = mal
>     negate = neg
>     abs = absZZ
>     signum = signumZZ
>     fromInteger = von_Zett_nach_ZZ
>
> instance Show ZZ where
>     show = showZZ

> absZZ :: ZZ -> ZZ
> absZZ (Minus n) = Plus n
> absZZ a = a
> 
> signumZZ :: ZZ -> ZZ
> signumZZ Null = Null
> signumZZ (Plus _) = Plus Eins
> signumZZ (Minus _) = Minus Eins

A.2

> showZZ :: ZZ -> String
> showZZ Null = "0"
> showZZ z@(Minus _) = "-" ++ showPositiveZZ (neg z)
> showZZ z = showPositiveZZ z
>
> showPositiveZZ :: ZZ -> String
> showPositiveZZ = myFix (\rec z -> if z <= 0 then "" else rec (z `durch` 16) ++ [digit z])
>   where
>     digit = (cycle "0123456789ABCDEF" !!) . fromEnum
>     myFix f = let x = f x in x

A.3

> class Binaer a where
>     a_zu_binaer :: a -> String
>     binaer_zu_a :: String -> a
> 
> instance Binaer Integer where
>     a_zu_binaer = bitsFromInteger
>     binaer_zu_a = bitsToInteger
> 
> bitsFromInteger :: Integer -> String
> bitsFromInteger 0 = "0"
> bitsFromInteger n
>   | n < 0 = '-' : bitsFromInteger (-n)
>   | otherwise = go n []
>   where
>     go 0 xs = xs
>     go n xs = go (n `div` 2) $ (if even n then '0' else '1') : xs
> 
> bitsToInteger :: String -> Integer
> bitsToInteger str = case str of
>     ('-':xs) -> -foldl f 0 xs
>     xs -> foldl f 0 xs
>   where
>     f n '1' = 2 * n + 1
>     f n '0' = 2 * n

> instance Binaer ZZ where
>     a_zu_binaer = a_zu_binaer . von_ZZ_nach_Zett
>     binaer_zu_a = von_Zett_nach_ZZ . binaer_zu_a 

> instance Binaer IN_1 where
>     a_zu_binaer = a_zu_binaer . Plus
>     binaer_zu_a = to_IN_1 . max 1 . bitsToInteger

module Angabe3 where


Utility functions to convert between Integral and IN_1

> from_IN_1 :: Integral a => IN_1 -> a
> from_IN_1 Eins = 1
> from_IN_1 (Nf n) = 1 + from_IN_1 n
>
> to_IN_1 :: Integral a => a -> IN_1
> to_IN_1 n
>   | n == 1 = Eins
>   | n > 1 = Nf $ to_IN_1 (n - 1)

Utility functions for arithmetic operations

> neg :: ZZ -> ZZ
> neg Null = Null
> neg (Plus a) = Minus a
> neg (Minus a) = Plus a
>
> inc :: ZZ -> ZZ
> inc Null = Plus Eins
> inc (Plus n) = Plus $ Nf n
> inc (Minus Eins) = Null
> inc (Minus (Nf n)) = Minus n
>
> dec :: ZZ -> ZZ
> dec = neg . inc . neg

A function that acts like 'quot'.
Division by 0 is not defined!

> quotZZ :: ZZ -> ZZ -> ZZ
> quotZZ Null _ = Null
> quotZZ a@(Plus _) b@(Plus _)
>   | a `kleiner` b = Null
>   | otherwise = Plus Eins `plus` quotZZ (a `minus` b) b
> quotZZ a@(Plus _) b@(Minus _) = neg $ quotZZ a (neg b)
> quotZZ a@(Minus _) b = quotZZ (neg a) (neg b)

Comparison (basically instance Ord)

> compareZZ :: ZZ -> ZZ -> Ordering
> compareZZ Null Null = EQ
> compareZZ Null (Plus _) = LT
> compareZZ Null (Minus _) = GT
> compareZZ (Plus _) Null = GT
> compareZZ (Minus _) Null = LT
> compareZZ (Minus _) (Plus _) = LT
> compareZZ (Plus _) (Minus _) = GT
> compareZZ a@(Minus _) b@(Minus _) = compareZZ (inc a) (inc b)
> compareZZ a@(Plus _) b@(Plus _) = compareZZ (dec a) (dec b)

> compareZZWith :: (Ordering -> Bool) -> ZZ -> ZZ -> Bool
> compareZZWith f a b = f (compareZZ a b)


A.1 Konvertierungsfunktionen

> von_Zett_nach_ZZ :: Zett -> ZZ
> von_Zett_nach_ZZ n
>   | n == 0 = Null
>   | n > 0 = Plus $ to_IN_1 n
>   | n < 0 = Minus $ to_IN_1 (-n)

> von_ZZ_nach_Zett :: ZZ -> Zett
> von_ZZ_nach_Zett Null = 0
> von_ZZ_nach_Zett (Plus n) = from_IN_1 n
> von_ZZ_nach_Zett (Minus n) = -from_IN_1 n


A.2 Operationen

> plus :: ZZ -> ZZ -> ZZ
> plus Null a = a
> plus a Null = a
> plus a@(Plus _) b = plus (dec a) (inc b)
> plus a@(Minus _) b = plus (inc a) (dec b)

> minus :: ZZ -> ZZ -> ZZ
> minus = flip $ plus . neg

> mal :: ZZ -> ZZ -> ZZ
> mal Null _ = Null
> mal _ Null = Null
> mal (Plus Eins) a = a
> mal a@(Plus _) b = b `plus` mal (dec a) b
> mal a@(Minus _) b = mal (neg a) (neg b)

This is euclidean division.
https://stackoverflow.com/q/24209927

> durch :: ZZ -> ZZ -> ZZ
> durch _ Null = Null -- so special
> durch x@(Minus _) y@(Minus _) = inc $ quotZZ (inc x) y
> durch x@(Minus _) y = dec $ quotZZ (inc x) y
> durch x y = quotZZ x y

> gleich :: ZZ -> ZZ -> Bool
> gleich = compareZZWith (==EQ)

> ungleich :: ZZ -> ZZ -> Bool
> ungleich = compareZZWith (/=EQ)

> groesser :: ZZ -> ZZ -> Bool
> groesser = compareZZWith (==GT)

> kleiner :: ZZ -> ZZ -> Bool
> kleiner = compareZZWith (==LT)

> ggleich :: ZZ -> ZZ -> Bool
> ggleich = compareZZWith (/=LT)

> kgleich :: ZZ -> ZZ -> Bool
> kgleich = compareZZWith (/=GT)

