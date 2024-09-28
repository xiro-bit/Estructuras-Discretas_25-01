
--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud(x:xs)= 1 + longitud(xs) 

sumaLista :: Num a => [a] -> a
sumaLista []=0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento xs x condicion = if condicion
                                then x :xs
                                else 
                                   xs++[x] 

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = if x > maximoLista xs
                     then x
                     else maximoLista xs

indice :: [a] -> Int -> a
indice[x] 0 = x
indice(x:xs) i = if i == 0
                then x
                else indice xs (i -1)
--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores n = [a | a <- [1..n], n `mod` a == 0]

conjunto :: Eq a => [a] -> [a]
conjunto []=[]
conjunto (x:xs)= x:conjunto[y|y <- xs, y/=x]

numerosPares :: [Int] -> [Int]
numerosPares (x:xs) = [x | x <- (x:xs), x `mod` 2 == 0]
