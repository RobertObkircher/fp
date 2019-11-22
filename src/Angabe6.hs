module Angabe6 where

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
-- Angabe 6:
--

type Adresse = Int
type Sprungadresse = Adresse
data Anweisung
    = AZ Arith_Variable Arith_Ausdruck -- Wertzuweisung an arithmetische Variable
    | LZ Log_Variable Log_Ausdruck -- Wertzuweisung an logische Variable
    | FU Log_Ausdruck Sprungadresse Sprungadresse -- Fallunterscheidung
    | BS Log_Ausdruck Sprungadresse -- Bedingter Sprung
    | US Sprungadresse -- Unbedingter Sprung
    | MP Adresse Anweisung -- Selbstmodifikation des Programms

type Zustand = Variablenbelegung
type Anfangszustand = Zustand
type Endzustand = Zustand
type Zwischenzustand = Zustand
type Programm = [Anweisung]
type EPS = Programm

--
--
--

interpretiere_1 :: EPS -> Anfangszustand -> Endzustand
interpretiere_1 e = last . interpretiere_2 e

interpretiere_2 :: EPS -> Anfangszustand -> [Zwischenzustand]
interpretiere_2 eps state = map getState $ takeWhile running (iterate step (newExecution eps state))

--
--
--

data Execution 
    = Execution 
    { before :: [Anweisung] 
    , instruction :: Anweisung
    , after :: [Anweisung]
    , index :: Int
    , state :: Zustand
    }
    | Terminated

running :: Execution -> Bool
running Terminated = False
running _ = True

newExecution :: [Anweisung] -> Anfangszustand -> Execution
newExecution [] _ = Terminated
newExecution (x:xs) state = Execution [] x xs 0 state

incPC :: Execution -> Execution
incPC Terminated = Terminated
incPC (Execution _ _ [] _ _) = Terminated
incPC (Execution l current (x:xs) n state) = Execution (current:l) x xs (n + 1) state

decPC :: Execution -> Execution
decPC Terminated = Terminated
decPC (Execution [] _ _ _ _) = Terminated
decPC (Execution (x:xs) current r n state) = Execution xs x (current:r) (n - 1) state

setPC :: Int -> Execution -> Execution
setPC n pc@(Execution _ _ _ index _)
  | n > index = incPC pc
  | n < index = decPC pc
setPC _ pc = pc

getState :: Execution -> Zustand
getState (Execution _ _ _ _ state) = state

step :: Execution -> Execution
step e = case instruction e of
  AZ arithVar arithExpr -> e {
    state = state e
  }
--AZ Arith_Variable Arith_Ausdruck
--LZ Log_Variable Log_Ausdruck -- Wertzuweisung an logische Variable
--FU Log_Ausdruck Sprungadresse Sprungadresse -- Fallunterscheidung
--BS Log_Ausdruck Sprungadresse -- Bedingter Sprung
--US Sprungadresse -- Unbedingter Sprung
--MP Adresse Anweisung -- Selbstmodifikation des Programms

--Arith_Variablenbelegung = Arith_Variable -> Int -- Total definierte Abb.
--Log_Variablenbelegung = Log_Variable -> Bool -- Total definierte Abb.
--Variablenbelegung = (Arith_Variablenbelegung,Log_Variablenbelegung)
--
