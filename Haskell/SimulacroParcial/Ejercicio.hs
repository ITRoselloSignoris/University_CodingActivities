module SolucionT1 where

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:y:xs) | fst x == snd x || (fst x == fst y || fst x == snd y) && (snd x == snd y || snd x == fst y) = False
                           | otherwise = relacionesValidas xs

---

personas :: [(String, String)] -> [String]
personas [] = []
personas  xs = quitarRepetidos (armarlista xs)

pertenece :: String -> [String] -> [String]
pertenece n [] = [n]
pertenece n (x:xs)| n == x = []
                  | otherwise = pertenece n xs

armarlista :: [(String, String)] -> [String]
armarlista [] = []
armarlista (x:xs) = [fst x] ++ [snd x] ++ armarlista xs 

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos (x:xs) = pertenece x xs ++ quitarRepetidos xs

---

amigosDe :: String -> [(String, String)] -> [String]
amigosDe "nadie" [] = ["nadie"]
amigosDe _ [] = [] -- esta mal. lo tenes que corregir
amigosDe nombre (x:xs) | fst x == nombre = snd x : amigosDe nombre xs
                       | snd x == nombre = fst x : amigosDe nombre xs
                       | otherwise = amigosDe nombre xs

---

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos xs = masApariciones "5" (armarlista xs) (armarlista xs)

contador :: String -> [String] -> Integer
contador n [] = 0
contador n (x:xs)| n == x = contador n xs + 1
                  | otherwise = contador n xs 

comparacionApariciones :: String -> String -> [String]-> String
comparacionApariciones n1 n2 [] = []
comparacionApariciones n1 n2 xs| contador n1 xs > contador n2 xs = n1 
              | otherwise = n2

masApariciones :: String -> [String] -> [String] -> String
masApariciones n [] _= n
masApariciones n (x:xs) ls| xs == [x] = masApariciones (comparacionApariciones n x ls) xs ls
                          | (comparacionApariciones n x ls) == x = masApariciones x xs ls
                          | otherwise = masApariciones n xs ls