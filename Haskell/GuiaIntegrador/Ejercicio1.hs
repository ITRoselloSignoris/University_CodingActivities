type Productos = [String]

generarStock :: Productos -> [(String,Integer)]
generarStock n = stockSinrepetidos n n


stockSinrepetidos :: Productos -> Productos -> [(String,Integer)]
stockSinrepetidos _ [] = []
stockSinrepetidos n (x:xs) | not(pertenece x xs) = (x, cantidadApariciones x n ): stockSinrepetidos n xs
                           | otherwise = stockSinrepetidos n xs

pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

cantidadApariciones :: String -> Productos -> Integer
cantidadApariciones _ [] = 0
cantidadApariciones n (x:xs) | n == x = cantidadApariciones n xs + 1
                             | otherwise = cantidadApariciones n xs

-- 

stockDeProducto :: [(String, Integer)] -> String -> Integer
stockDeProducto [] _ = 0
stockDeProducto (x:xs) producto | producto == fst x = snd x  
                                | otherwise = stockDeProducto xs producto

--

dineroEnStock :: [(String, Integer)] ->[(String, Float)] -> Float
dineroEnStock  [] _ = 0
dineroEnStock (x:xs) precios = dineroEnStock xs precios + sumatoria x precios 

sumatoria :: (String, Integer) -> [(String, Float)] -> Float
sumatoria _ [] = 0
sumatoria n (x:xs) | fst n == fst x = sumatoria n xs + (fromIntegral (snd n) * snd x)
                   | otherwise = sumatoria n xs

--

aplicarOferta :: [(String, Integer)] ->[(String, Float)] ->[(String,Float)]
aplicarOferta _ []= []
aplicarOferta  stock (x:xs) = armarPrecios x stock stock ++ aplicarOferta stock xs


armarPrecios :: (String, Float) -> [(String, Integer)] -> [(String, Integer)] -> [(String,Float)]
armarPrecios _ _ [] = []
armarPrecios n productos (x:xs)| fst n ==  fst x && stockDeProducto productos (fst x) > 10 = [(fst n, snd n * 0.80)]
                               | fst n ==  fst x = [n]
                               | otherwise = armarPrecios n productos xs

