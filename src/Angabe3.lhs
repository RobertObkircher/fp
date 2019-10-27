> module Angabe3 where

> data IN_1 = Eins | Nf IN_1 deriving (Show)
> data ZZ = Null | Plus IN_1 | Minus IN_1 deriving (Show)

> type Zett = Integer


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

> quotZZ :: ZZ -> ZZ -> ZZ
> quotZZ _ Null = Null
> quotZZ Null _ = Null
> quotZZ a@(Plus _) b@(Plus _)
>   | a `kleiner` b = Null
>   | otherwise = Plus Eins `plus` quotZZ (a `minus` b) b
> quotZZ a@(Plus _) b@(Minus _) = neg $ quotZZ a (neg b)
> quotZZ a@(Minus _) b = quotZZ (neg a) b


Constants

> m = Plus Eins :: ZZ
> n = Minus Eins :: ZZ

Comparison

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
> mal a@(Plus _) b = mal (dec a) b `plus` b
> mal a@(Minus _) b = mal (neg a) (neg b)

> durch :: ZZ -> ZZ -> ZZ
> durch a@(Plus _) b@(Minus _) = dec $ quotZZ a b
> durch a@(Minus _) b@(Plus _) = dec $ quotZZ a b
> durch a b = quotZZ a b

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

