data Var = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Show, Eq, Ord)

data Formula = Atom Var
              | Neg Formula
              | Formula :&: Formula
              | Formula :|: Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom a) = [a]
variables (Neg f) = eliminarDuplicados (variables f)
variables (f1 :&: f2) = eliminarDuplicados (variables f1 ++ variables f2)
variables (f1 :|: f2) = eliminarDuplicados (variables f1 ++ variables f2)
variables (f1 :=>: f2) = eliminarDuplicados (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = eliminarDuplicados (variables f1 ++ variables f2)

eliminarDuplicados :: Eq a => [a] -> [a]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) =
    if estaContenido xs x 
    then eliminarDuplicados xs 
    else x : eliminarDuplicados xs

estaContenido :: Eq a => [a] -> a -> Bool
estaContenido [] _ = False
estaContenido (x:xs) y =
    if x == y 
    then True 
    else estaContenido xs y
-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom a) = Neg (Atom a)
negacion (Neg f) = f
negacion (f1 :&: f2) = negacion f1 :|: negacion f2
negacion (f1 :|: f2) = negacion f1 :&: negacion f2
negacion (f1 :=>: f2) = negacion f1 :|: negacion f2
negacion (f1 :<=>: f2) = (negacion f1 :&: negacion f2) :|: (f1 :&: negacion f2)
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom a) = Atom a
equivalencia (Neg f) = Neg (equivalencia f)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2
equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :=>: f2) = Neg (equivalencia f1) :|: equivalencia f2 
equivalencia (f1 :<=>: f2) =
    ((equivalencia f1 :&: equivalencia f2) 
     :|:
     (Neg (equivalencia f1) :&: Neg (equivalencia f2)))
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
fotra :: Var -> [(Var, Bool)] -> Bool
fotra v [] = False  
fotra v ((x, val):xs) =
    if v == x 
    then val 
    else fotra v xs

interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom v) vals = fotra v vals
interpretacion (Neg f) vals = not $ interpretacion f vals 
interpretacion (f1 :&: f2) vals = interpretacion f1 vals && interpretacion f2 vals
interpretacion (f1 :|: f2) vals = interpretacion f1 vals || interpretacion f2 vals


interpretacion (f1 :=>: f2) vals =
    not (interpretacion f1 vals) || interpretacion f2 vals 
interpretacion (f1 :<=>: f2) vals =
    interpretacion f1 vals == interpretacion f2 vals
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones formula =
    generarCombinaciones $ variables formula 

generarCombinaciones :: [Var] -> [[(Var, Bool)]]
generarCombinaciones [] = [[]]
generarCombinaciones (v:vs) =
    [(v, False):cs | cs <- generarCombinaciones vs] ++ [(v, True):cs  | cs <- generarCombinaciones vs]

-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------
tablaDeVerdad :: Formula -> [([(Var,Bool)], Bool)]
tablaDeVerdad formula =
    [(asignación, interpretacion formula asignación) | asignación <- combinaciones formula]
-----------------------------------------------------
