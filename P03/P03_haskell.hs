data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node a xs) = 1 + longitud xs

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void a= False
estaContenido (Node x xs) a = if x==a
                            then True
                            else estaContenido(xs) a


convertirAEstructura :: [a] -> List a
convertirAEstructura[]= Void
convertirAEstructura (x:xs) = (Node x (convertirAEstructura xs))

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node a xs)= a:(convertirALista xs)
                            
conjunto :: Eq a => List a -> List a
conjunto Void= Void
conjunto (Node a xs) = if estaContenido xs a
                        then conjunto xs
                        else Node a (conjunto xs)


eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = Void
eliminarIndice (Node _ xs) 0 = xs
eliminarIndice (Node a xs) x =  if x > 0 && x < longitud (Node a xs)
                                 then Node a (eliminarIndice xs (x - 1))
                                 else error "índice fuera del rango permitido"
                         
                                

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void 0 valor = Node valor Void
insertarIndice Void _ valor = error "índice fuera del rango permitido" 
insertarIndice (Node x xs) 0 valor = Node valor (Node x xs)
insertarIndice (Node x xs) indice valor = if indice > 0 && indice <= longitud (Node x xs)
                                             then Node x (insertarIndice xs (indice - 1) valor)
                                             else error "índice fuera del rango permitido"
 

recorrerLista :: List a -> Int -> List a
recorrerLista Void _ = Void
recorrerLista (Node a xs) 0 = Node a xs
recorrerLista (Node a xs) indice = recorrerLista (insertarIndice xs (longitud xs) a) (indice - 1)

                            
 