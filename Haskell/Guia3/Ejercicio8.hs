absoluto3 :: Integer -> Integer 
absoluto3 n | n < 0 = -n
           | otherwise = n

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = mod (absoluto3 x) 10 +  mod (div (absoluto3 x) 10) 10

comparar :: Integer -> Integer -> Integer
comparar x y | sumaUltimosDosDigitos x < sumaUltimosDosDigitos y = 1
             | sumaUltimosDosDigitos x > sumaUltimosDosDigitos y = -1
             | otherwise = 0