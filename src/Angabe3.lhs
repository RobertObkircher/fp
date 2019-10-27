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

Constants

> m = Plus Eins :: ZZ
> n = Minus Eins :: ZZ


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
> mal a@(Plus _) b = mal (dec a) b `plus` b
> mal a@(Minus _) b = mal (neg a) (neg b)

> durch :: ZZ -> ZZ -> ZZ
> durch _ Null = Null
> durch Null _ = Null
> durch a@(Plus _) b@(Plus _)
>   | a `kleiner` b = Null
>   | otherwise = Plus Eins `plus` durch (a `minus` b) b
> durch a@(Plus _) b@(Minus _) = neg $ durch a (neg b)
> durch a@(Minus _) b = durch (neg a) b


> gleich :: ZZ -> ZZ -> Bool
> gleich n = not . ungleich n

> ungleich :: ZZ -> ZZ -> Bool
> ungleich a b = kleiner a b || kleiner b a

> groesser :: ZZ -> ZZ -> Bool
> groesser a b = ungleich a b && not (kleiner a b)

> kleiner :: ZZ -> ZZ -> Bool
> kleiner Null Null = False
> kleiner s = 9
> kleiner Null (Plus _) = True
> kleiner Null (Minus _) = False
> kleiner a@(Minus _) b@(Plus _) = True
> kleiner a@(Minus _) Null = True
> kleiner a@(Plus _) b@(Minus _) = False
> kleiner a@(Plus _) Null = False
> kleiner a@(Minus _) b@(Minus _) = kleiner (inc a) (inc b)
> kleiner a@(Plus _) b@(Plus _) = kleiner (dec a) (dec b)


> ggleich :: ZZ -> ZZ -> Bool
> ggleich n = not . kleiner n

> kgleich :: ZZ -> ZZ -> Bool
> kgleich n = not . groesser n

