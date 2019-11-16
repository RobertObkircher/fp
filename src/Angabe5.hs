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
wird_gestreikt m n = streiksProTag m !! (n - 1) >= 1

--
--

streiksProTag :: Modellszenario -> [Nat0]
streiksProTag = map anzahlParteien . tage

data Tag = Tag Wochentag Anzahl_Parteien deriving (Eq, Show)

notInWochentage :: [Wochentag] -> Tag -> Bool
notInWochentage ws (Tag w _) = w `notElem` ws

anzahlParteien :: Tag -> Anzahl_Parteien
anzahlParteien (Tag _ a) = a

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
--




