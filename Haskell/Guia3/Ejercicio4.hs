--a

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt x y | True = fst x * fst y + snd x * snd y 
            | otherwise = undefined

--b
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor x y | fst x < fst y && snd x < snd y = True
              | otherwise = False

--c

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)

--d

sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x, y, z) = x + y + z 

--e
esMultiploDe2 :: Integer -> Integer -> Integer
esMultiploDe2 x y | mod x y == 0 = 1
                  | otherwise = 0

sumaSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumaSoloMultiplos (x, y, z) n = esMultiploDe2 x n * x + esMultiploDe2 y n * y + esMultiploDe2 z n * z 