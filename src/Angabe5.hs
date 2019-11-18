module Angabe5 where

type Nat0 = Int
type Nat1 = Int

data Wochentag = Mo | Di | Mi | Do | Fr | Sa | So deriving (Eq,Show,Enum)

type Starttag = Wochentag
type Zeitraum_in_Tagen = Nat1
type Streikaufrufabstand_in_Tagen = Nat1

newtype Partei = P Streikaufrufabstand_in_Tagen deriving Show

type Parteien = [Partei]
type Streiktage = Nat0
type Anzahl_Parteien = Nat1
type Modellszenario = (Starttag,Zeitraum_in_Tagen,Parteien)

streiktage :: Modellszenario -> Streiktage
streiktage = flip grossstreiktage 1

superstreiktage :: Modellszenario -> Streiktage
superstreiktage m@(_,_,parteien) = grossstreiktage m $ length parteien

grossstreiktage :: Modellszenario -> Anzahl_Parteien -> Streiktage
grossstreiktage m anzahl = length $ filter (>=anzahl) $ streiksProTag m

streiktage_am :: Modellszenario -> Wochentag -> Anzahl_Parteien -> Streiktage
streiktage_am m tag anzahl = sum $ map f $ tage m
  where
    f :: Tag -> Anzahl_Parteien
    f (Tag t a)
      | t == tag && a >= anzahl = 1
      | otherwise = 0

wird_gestreikt :: Modellszenario -> Nat1 -> Bool
wird_gestreikt m n = nth n (streiksProTag m) >= 1
  where
    nth :: Int -> [Int] -> Int
    nth _ [] = 0
    nth 1 (x:xs) = x
    nth n (_:xs) = nth (n - 1) xs

--
--

-- removes the Tag constructor
streiksProTag :: Modellszenario -> [Nat0]
streiksProTag = map anzahlParteien . tage

-- stores the number of parties per day
data Tag = Tag Wochentag Anzahl_Parteien deriving (Eq, Show)

-- used to filter Fridays and Sundays
notInWochentage :: [Wochentag] -> Tag -> Bool
notInWochentage ws (Tag w _) = w `notElem` ws

-- how many parties
anzahlParteien :: Tag -> Anzahl_Parteien
anzahlParteien (Tag _ a) = a

-- the simulation
tage :: Modellszenario -> [Tag]
tage m@(start, schritte, parteien) = map tag [1..schritte]
  where
    tag :: Nat1 -> Tag
    tag n = Tag d (if d == Fr || d == So then 0 else anzahl)
      where
        d :: Wochentag
        d = toEnum $ (n + fromEnum start - 1) `rem` 7
        streikt :: Partei -> Bool
        streikt (P p) = n `rem` p == 0
        anzahl :: Nat0
        anzahl = length $ filter streikt parteien

--
-- A2
--

data Arith_Variable = A1 | A2 | A3 | A4 | A5 | A6 deriving (Eq,Show)
data Log_Variable = L1 | L2 | L3 | L4 | L5 | L6 deriving (Eq,Show)

data Arith_Ausdruck
    = AK Int -- Arithmetische Konstante
    | AV Arith_Variable -- Arithmetische Variable
    | Plus Arith_Ausdruck Arith_Ausdruck -- Addition
    | Minus Arith_Ausdruck Arith_Ausdruck -- Subtraktion
    | Mal Arith_Ausdruck Arith_Ausdruck -- Multiplikation
    deriving (Eq,Show)

data Log_Ausdruck
    = LK Bool -- Logische Konstante
    | LV Log_Variable -- Logische Variable
    | Nicht Log_Ausdruck -- Logische Negation
    | Und Log_Ausdruck Log_Ausdruck -- Logische Konjunktion
    | Oder Log_Ausdruck Log_Ausdruck -- Logische Disjunktion
    | Gleich Arith_Ausdruck Arith_Ausdruck -- Wertgleichheit arith. Ausdruecke
    | Kleiner Arith_Ausdruck Arith_Ausdruck -- Linker Ausdruck
                                            -- echt wertkleiner
                                            -- als rechter
                                            -- Ausdruck
    deriving (Eq,Show)

type Arith_Variablenbelegung = Arith_Variable -> Int -- Total definierte Abb.
type Log_Variablenbelegung = Log_Variable -> Bool -- Total definierte Abb.
type Variablenbelegung = (Arith_Variablenbelegung,Log_Variablenbelegung)

links :: Either a b -> a
links (Left x) = x

rechts :: Either a b -> b
rechts (Right y) = y

class Evaluierbar a where
    evaluiere :: a -> Variablenbelegung -> Either Int Bool

--
--

instance Evaluierbar Arith_Ausdruck where
    evaluiere a = Left . evaluiereArithAusdruck a

instance Evaluierbar Log_Ausdruck where
    evaluiere a = Right . evaluiereLogAusdruck a

--
--


-- Recursively evaluates an arithmetic expression
evaluiereArithAusdruck :: Arith_Ausdruck -> Variablenbelegung -> Int
evaluiereArithAusdruck a vars@(avars,_) = case a of
    AK int -> int
    AV var -> avars var
    Plus l r -> rec l + rec r
    Minus l r -> rec l - rec r
    Mal l r -> rec l * rec r
  where
    rec :: Arith_Ausdruck -> Int
    rec = flip evaluiereArithAusdruck vars

-- Recursively evaluate a logical expression
evaluiereLogAusdruck :: Log_Ausdruck -> Variablenbelegung -> Bool
evaluiereLogAusdruck a vars@(_, lvars) = case a of
    LK b -> b
    LV var -> lvars var
    Nicht a -> not $ rec a
    Und l r -> rec l && rec r
    Oder l r -> rec l || rec r
    Gleich l r -> arith l == arith r
    Kleiner l r -> arith l < arith r
  where
    rec :: Log_Ausdruck -> Bool
    rec = flip evaluiereLogAusdruck vars
    arith :: Arith_Ausdruck -> Int
    arith = flip evaluiereArithAusdruck vars

--
