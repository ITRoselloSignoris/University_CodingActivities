-- Ejercicio 1
--1 
longitud :: [t] -> Integer
longitud n | null n  = 0
           | otherwise =  1 + longitud  (tail n)

--2
ultimo :: [t] -> t
ultimo n | 1 == longitud n = head n
         | otherwise = ultimo((tail n))
--3





--4
reverso :: [t] -> [t]
reverso [] = []
reverso n
        | otherwise =(reverso(tail n))++[head n]


--Ejercicio2
--1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n xs| xs == [] = False 
              | head xs == n = True
              | otherwise = pertenece n (tail xs)
--2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | x /= y = False
                      | otherwise = todosIguales xs
--3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos (x:xs)| pertenece x xs == True = False
                     | otherwise = todosDistintos xs 
--4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs == True = True
                    | otherwise = hayRepetidos xs
--5
quitar :: (Eq t ) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = [x]++quitar n xs
--6
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []  
quitarTodos n xs| pertenece n xs == False = xs
                | otherwise = quitar n [head xs] ++ quitarTodos n (quitar n (tail xs))
--7






--8





--9



-- Ejercicio3

--1



--2







--4




--5




--6







--7






--8








--Ejercicio4

