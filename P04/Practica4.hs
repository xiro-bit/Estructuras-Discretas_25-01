data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) =
    1 + (if profundidad izq > profundidad der 
         then profundidad izq 
         else profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PostOrder deriving Show

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz valor izq der) InOrder = recorrido izq InOrder ++ [valor] ++ recorrido der InOrder
recorrido (Raiz valor izq der) PreOrder = [valor] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz valor izq der) PostOrder = recorrido izq PostOrder ++ recorrido der PostOrder ++ [valor]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles arbol =
    nivelesAux [arbol]

nivelesAux :: [Arbol a] -> [[a]]
nivelesAux [] = []
nivelesAux nodos = valores nodos : nivelesAux (hijos nodos)

valores :: [Arbol a] -> [a]
valores [] = []
valores (ArbolVacio : xs) = valores xs
valores ((Raiz v _ _) : xs) = v : valores xs

hijos :: [Arbol a] -> [Arbol a]
hijos [] = []
hijos (ArbolVacio : xs) = hijos xs
hijos ((Raiz _ izq der) : xs) = izq : der : hijos xs

-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a 
minimo (Raiz v ArbolVacio _) = v
minimo (Raiz _ izq _) = minimo izq
minimo ArbolVacio = error "El árbol está vacío"

-------------------- EJERCICIO 6 --------------------
maximo :: Ord a => Arbol a -> a 
maximo (Raiz v _ ArbolVacio) = v
maximo (Raiz _ _ der) = maximo der
maximo ArbolVacio = error "El árbol está vacío"

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio _ = error "El árbol está vacío"