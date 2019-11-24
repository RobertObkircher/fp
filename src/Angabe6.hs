module Angabe6 where

data Arith_Variable = A1 | A2 | A3 | A4 | A5 | A6 deriving (Eq,Show,Enum)
data Log_Variable = L1 | L2 | L3 | L4 | L5 | L6 deriving (Eq,Show,Enum)

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
type Variablenbelegung = (Arith_Variablenbelegung, Log_Variablenbelegung)

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
evaluiereArithAusdruck a vars@(avars, _) = case a of
    AK int    -> int
    AV var    -> avars var
    Plus  l r -> rec l + rec r
    Minus l r -> rec l - rec r
    Mal   l r -> rec l * rec r
  where
    rec :: Arith_Ausdruck -> Int
    rec = flip evaluiereArithAusdruck vars

-- Recursively evaluate a logical expression
evaluiereLogAusdruck :: Log_Ausdruck -> Variablenbelegung -> Bool
evaluiereLogAusdruck a vars@(_, lvars) = case a of
    LK    b     -> b
    LV    var   -> lvars var
    Nicht a     -> not $ rec a
    Und     l r -> rec l && rec r
    Oder    l r -> rec l || rec r
    Gleich  l r -> arith l == arith r
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
    deriving (Show)

type Zustand = Variablenbelegung
type Anfangszustand = Zustand
type Endzustand = Zustand
type Zwischenzustand = Zustand
type Programm = [Anweisung]
type EPS = Programm

--
-- A1
--

interpretiere_1 :: EPS -> Anfangszustand -> Endzustand
interpretiere_1 e = last . interpretiere_2 e

interpretiere_2 :: EPS -> Anfangszustand -> [Zwischenzustand]
interpretiere_2 eps s = map (state . fst) $ filter snd (zip execs keep)
  where
    execs = execute $ newExecution eps s
    keep  = True : map isAssignment execs

isAssignment :: Execution -> Bool
isAssignment e = case instruction (pc e) of
    Just (AZ _ _) -> True
    Just (LZ _ _) -> True
    _             -> False

--
--
--

data ProgramCounter
    = PC [Anweisung] !Anweisung [Anweisung] !Adresse !Int
    | Terminated

newPC :: [Anweisung] -> ProgramCounter
newPC []                    = Terminated
newPC instructions@(x : xs) = PC [] x xs 0 (length instructions)

incPC :: ProgramCounter -> ProgramCounter
incPC Terminated      = Terminated
incPC (PC _ _ [] _ _) = Terminated
incPC (PC left current (x : xs) n count) =
    PC (current : left) x xs (n + 1) count

decPC :: ProgramCounter -> ProgramCounter
decPC Terminated      = Terminated
decPC (PC [] _ _ _ _) = Terminated
decPC (PC (x : xs) current right index count) =
    PC xs x (current : right) (index - 1) count

goto :: Int -> ProgramCounter -> ProgramCounter
goto n pc@(PC _ _ _ index count) | n >= count = Terminated
                                 | n < 0      = Terminated
                                 | n > index  = goto n $ incPC pc
                                 | n < index  = goto n $ decPC pc
goto _ pc = pc

gotoOrAppend :: Int -> ProgramCounter -> ProgramCounter
gotoOrAppend n pc@(PC left current right index count)
    | n /= count         = goto n pc
    | index /= lastIndex = gotoOrAppend n $ goto lastIndex pc
    | index == lastIndex = PC (current : left) (US 42) [] n n
  where
    lastIndex :: Adresse
    lastIndex = count - 1

instruction :: ProgramCounter -> Maybe Anweisung
instruction Terminated           = Nothing
instruction (PC _ current _ _ _) = Just current

setInstruction :: Anweisung -> ProgramCounter -> ProgramCounter
setInstruction _ Terminated = Terminated
setInstruction instr (PC left _ right index count) =
    PC left instr right index count

--
--
--

data Execution = Execution
  { pc :: ProgramCounter
  , state :: Zustand
  }

running :: Execution -> Bool
running e = case pc e of
    Terminated -> False
    _          -> True

newExecution :: [Anweisung] -> Anfangszustand -> Execution
newExecution = Execution . newPC

modifyState :: (Zustand -> Zustand) -> Execution -> Execution
modifyState f e = e { state = f (state e) }

modifyPC :: (ProgramCounter -> ProgramCounter) -> Execution -> Execution
modifyPC f e = e { pc = f (pc e) }

execute :: Execution -> [Execution]
execute = takeWhile' running . iterate step

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> x : if p x then xs else []) []

--
--
--

step :: Execution -> Execution
step e = case instruction (pc e) of
    Nothing    -> e
    Just instr -> case instr of
        AZ arithVar arithExpr ->
            modifyPC incPC $ modifyState (setArithVariable arithVar arithExpr) e
        LZ logVar logExpr ->
            modifyPC incPC $ modifyState (setLogVariable logVar logExpr) e
        FU logExpr addrL addrR -> modifyPC
            (if evaluiereLogAusdruck logExpr (state e)
                then goto addrL
                else goto addrR
            )
            e
        BS logExpr addr -> modifyPC
            (if evaluiereLogAusdruck logExpr (state e) then goto addr else incPC
            )
            e
        US addr       -> modifyPC (goto addr) e
        MP addr instr -> modifyPC (setInstruction instr . gotoOrAppend addr) e

setArithVariable
    :: Arith_Variable
    -> Arith_Ausdruck
    -> Variablenbelegung
    -> Variablenbelegung
setArithVariable var value s@(arithVars, logVars) = (arithVars', logVars)
  where
    arithVars' :: Arith_Variable -> Int
    arithVars' v =
        if v == var then evaluiereArithAusdruck value s else arithVars v

setLogVariable
    :: Log_Variable -> Log_Ausdruck -> Variablenbelegung -> Variablenbelegung
setLogVariable var value s@(arithVars, logVars) = (arithVars, logVars')
  where
    logVars' :: Log_Variable -> Bool
    logVars' v = if v == var then evaluiereLogAusdruck value s else logVars v

--
-- A2
--

gib_aus_arith_Varbel :: Arith_Variablenbelegung -> [(Arith_Variable, Int)]
gib_aus_arith_Varbel v = map (\l -> (l, v l)) [A1 .. A6]

gib_aus_log_Varbel :: Log_Variablenbelegung -> [(Log_Variable, Bool)]
gib_aus_log_Varbel v = map (\l -> (l, v l)) [L1 .. L6]

gib_aus_Zustand :: Zustand -> ([(Arith_Variable, Int)], [(Log_Variable, Bool)])
gib_aus_Zustand (a, l) = (gib_aus_arith_Varbel a, gib_aus_log_Varbel l)


--
-- A3
--


ggt :: EPS
ggt =
    [ BS (Gleich a1 a2)          6
    , BS (Nicht (Kleiner a2 a1)) 4
    , AZ A1 $ Minus a1 a2
    , US 0
    , AZ A2 $ Minus a2 a1
    , US 0
    , AZ A3 a1
    ]
  where
    a1 = AV A1
    a2 = AV A2

fibo :: EPS
fibo =
    [ AZ A6 k0
    , AZ A5 $ AK 1
    , LZ L1 $ Nicht $ Kleiner a1 k0
    , BS l1 loop
    , AZ A1 (Minus k0 a1)
    , BS (Gleich a1 k0) (length fibo - 2) -- loop
    , AZ A4 (Plus a6 a5)
    , AZ A6 a5
    , AZ A5 a4
    , US loop
    , BS l1 (length fibo)
    , AZ A6 (Minus k0 a6)
    ]
  where
    l1   = LV L1 -- not negative
    a1   = AV A1 -- loop counter
    a4   = AV A4 -- fib(n+2) tmp
    a5   = AV A5 -- fib(n+1)
    a6   = AV A6 -- fib(n)
    k0   = AK 0
    loop = 5


azst1 :: Anfangszustand
azst1 = (enumIndex ([24,60] ++ repeat 0), const True)

azst2 :: Anfangszustand
azst2 = (enumIndex ([18, 45] ++ [3..]), enumIndex (cycle [False, True]))

generiere :: [Int] -> [Bool] -> Zustand
generiere is bs = (enumIndex (is ++ repeat 0), enumIndex (bs ++ repeat False))

enumIndex :: Enum i => [a] -> i -> a
enumIndex xs i = xs !! fromEnum i
