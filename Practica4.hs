--López Rojas Jesús
--Pineda Morales Roberto Gael
--Practica 4

data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud ArbolVacio = 0 
longitud (Raiz a arbi arbd) = 1 + longitud arbi + longitud arbd 

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad ArbolVacio = 0
profundidad (Raiz a arbi arbd) = 1 + max(profundidad arbi) (profundidad arbd)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho ArbolVacio = 0
ancho (Raiz a ArbolVacio ArbolVacio) = 1
ancho (Raiz a arbi arbd) = ancho arbi + ancho arbd

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz a arbi arbd) InOrder =  recorrido arbi InOrder ++ [a] ++ recorrido arbd InOrder
recorrido (Raiz a arbi arbd) PreOrder = [a] ++ recorrido arbi PreOrder ++ recorrido arbd PreOrder
recorrido (Raiz a arbi arbd) PosOrder =  recorrido arbi PosOrder ++ recorrido arbd PosOrder ++ [a]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz a ArbolVacio  ArbolVacio) = [[a]]
niveles (Raiz a arbolIzquierdo  arbolDerecho) = [a]
                                                   : combinarNiveles(niveles arbolIzquierdo) (niveles arbolDerecho)

combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x++y) : combinarNiveles xs ys

-------------------- EJERCICIO 6 --------------------
minimo :: Arbol a -> a 
minimo ArbolVacio = error "Está vacia, no hay minimos"
minimo (Raiz a ArbolVacio _) = a 
minimo (Raiz a arbolIzquierdo _) = minimo arbolIzquierdo

-------------------- EJERCICIO 7 --------------------
maximo :: Arbol a -> a 
maximo  ArbolVacio = error  "Esta vacia, no hay máximos"
maximo (Raiz a _ ArbolVacio) = a
maximo (Raiz a _ arbolDerecho) = maximo arbolDerecho

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio elemento = error "El árbol está vacío"

eliminar (Raiz x ArbolVacio arbolDerecho) elemento = if x == elemento
                                                        then arbolDerecho
                                                        else eliminar arbolDerecho elemento
eliminar (Raiz x arbolIzquierdo ArbolVacio) elemento = if x == elemento
                                                          then arbolIzquierdo
                                                          else eliminar arbolIzquierdo elemento
eliminar (Raiz x arbolIzquierdo arbolDerecho) elemento = if elemento < x
                                                            then (Raiz x (eliminar arbolIzquierdo elemento) arbolDerecho)
                                                            else if elemento > x
                                                                then (Raiz x (eliminar arbolDerecho elemento) arbolIzquierdo)
                                                                else (Raiz (minimo arbolDerecho) arbolIzquierdo(eliminar arbolDerecho (minimo arbolDerecho)))