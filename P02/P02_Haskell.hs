--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud(x:xs)= 1 + longitud(xs) 

sumaLista :: Num a => [a] -> a
sumaLista []=0
sumaLista (x:xs) = (x + sumaLista xs)

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] x condicion = if True
                                then x :[]
                                else 
                                   []++[x]  

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista _ = undefined

indice :: [a] -> Int -> a
indice[x]= 0
indice(x:xs)= if indice ==0
                then x
                else indice xs (indice - 1)
--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores _ = undefined

conjunto :: Eq a => [a] -> [a]
conjunto _ = undefined

numerosPares :: [Int] -> [Int]
numerosPares _ = undefined
