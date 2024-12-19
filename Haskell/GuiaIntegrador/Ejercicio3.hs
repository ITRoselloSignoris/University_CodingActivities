divisoresPropios :: Integer -> [Integer]
divisoresPropios n = divisores n n

divisores :: Integer -> Integer -> [Integer]
divisores 0 _ = []
divisores n m | mod m n == 0 && n /= m = n:divisores (n-1) m
              | otherwise = divisores (n-1) m


--

sonAmigos :: Integer -> Integer -> Bool
sonAmigos n m = sumaDivisores (divisoresPropios n) == m && sumaDivisores (divisoresPropios m) == n

sumaDivisores :: [Integer] -> Integer
sumaDivisores [] = 0
sumaDivisores (x:xs) = sumaDivisores xs + x


--

losPrimerosNPerfectos :: Integer -> [Integer]
losPrimerosNPerfectos 0 = []
losPrimerosNPerfectos n = nPerfectos n 1

nPerfectos :: Integer -> Integer -> [Integer]
nPerfectos 0 _ = []
nPerfectos n m | sumaDivisores(divisoresPropios m) == m = m : nPerfectos (n-1) (m+1)
               | otherwise = nPerfectos n (m+1)


--

listaDeAmigos :: [Integer] -> [(Integer,Integer)]
listaDeAmigos [] = []
listaDeAmigos lista = noRepetidos (recursion lista lista)

recursion :: [Integer] -> [Integer] -> [(Integer,Integer)]
recursion [] _ = []
recursion (x:xs) lista = recursion xs lista ++ listaAmigos x lista

listaAmigos :: Integer -> [Integer] -> [(Integer,Integer)]
listaAmigos _ [] = []
listaAmigos n (x:xs)| sonAmigos n x && n /= x = (n,x) : listaAmigos n xs 
                    | otherwise = listaAmigos n xs

noRepetidos :: [(Integer,Integer)] -> [(Integer,Integer)]
noRepetidos [] = []
noRepetidos (x:xs) | pertenece x xs = noRepetidos xs
                   | otherwise = x : noRepetidos xs

pertenece :: (Integer,Integer) -> [(Integer,Integer)] -> Bool
pertenece _ [] = True
pertenece n (x:xs)| iguales n x = False
                  | otherwise = pertenece n xs

iguales :: (Integer,Integer) -> (Integer,Integer) -> Bool
iguales n m = (fst n == fst m || fst n == snd m) && (snd n == fst m || snd n == snd m)