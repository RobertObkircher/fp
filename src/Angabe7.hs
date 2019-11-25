module Angabe7 where

--
-- A1
--

generiere_fak_strom :: [Integer]
generiere_fak_strom = 1 : zipWith (*) generiere_fak_strom [1..]

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
generiere_exp_strom = undefined

selektiere :: Genauigkeit -> Strom -> Approx_Wert
selektiere = undefined

--
-- A3
--

type Woerterstrom = [String]

generiere_woerter :: Woerterstrom
generiere_woerter = undefined

filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome = undefined


--
-- A4
--

type Wort = String
type Woerterbuch = [Wort] -- nur Werte endliche Laenge; keine Stroeme.
type Wortleiter = [Wort]

ist_aufsteigende_leiterstufe :: Wort -> Wort -> Bool
ist_aufsteigende_leiterstufe = undefined

ist_aufsteigende_wortleiter :: [Wort] -> Bool
ist_aufsteigende_wortleiter = undefined

gib_max_aufsteigende_wortleiter :: Woerterbuch -> Wortleiter
gib_max_aufsteigende_wortleiter = undefined

