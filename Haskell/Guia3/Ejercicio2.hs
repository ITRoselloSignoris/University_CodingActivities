--a
absoluto :: Integer -> Integer 
absoluto n | n < 0 = -n
           | otherwise = n

--b  ERROR

maximoabsoluto :: (Integer, Integer) -> Integer
maximoabsoluto (x,y) | absoluto x < absoluto y = absoluto y
                  | absoluto x > absoluto y = absoluto x
                  | otherwise = undefined

--c 
maximo3 :: Integer -> Integer -> Integer ->  Integer
maximo3 z x y | (x < z) && (z < y) = y 
              | (z > x) && (z > y) = z
              | (x > z) && (x > y) = x
              | otherwise = undefined

--d

algunoEs01 :: Float -> Float -> Bool
algunoEs01 x y | x == 0 || y == 0  = True
               | otherwise = False

algunoEs02 :: Float -> Float -> Bool
algunoEs02 x y = x == 0 || y == 0

--e 

ambosSon01 :: Float -> Float -> Bool
ambosSon01 x y | x == 0 && y == 0  = True
               | otherwise = False

ambosSon02 :: Float -> Float -> Bool
ambosSon02 x y = x == 0 && y == 0

--f 

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | x <= 3 && y <= 3 = True
                   | x <= 3 && x > 7 && y <= 3 && y > 7 = True
                   | x > 7 && y > 7 = True
                   | otherwise = False

--g

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y && y == z = 0
                    | x == y = z
                    | x == z = y
                    | z == y = x
                    | otherwise = x + y + z

--h

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False

--i

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod (absoluto x) 10

--j

digitoDecenas :: Integer -> Integer
digitoDecenas x = digitoUnidades (div x 10)