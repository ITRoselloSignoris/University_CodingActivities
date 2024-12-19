-- EJERCICIO 1--------
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = fibonacci (n-1) + fibonacci(n-2)


-- EJERCICIO 2--------
parteEntera :: Float -> Integer
parteEntera n | n >= 0 && n < 1 = 0
              | otherwise = 1 + parteEntera(n-1)


-- EJERCICIO 7--------
--Funcion Auxiliar
cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = cantDigitos(div n 10) + 1     

--Funcion Principal
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i  = mod(div n (10^(cantDigitos n - i)))10


-- EJERCICIO 9--------
--Funciones Auxiliares
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i  = mod(div n (10^(cantDigitos n - i)))10

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = cantDigitos(div n 10) + 1
sacarPrimerDigito :: Integer -> Integer
sacarPrimerDigito n = div n 10

sacarUltimoDigito :: Integer -> Integer
sacarUltimoDigito n = mod n (10 ^ ((cantDigitos n) -1))

-- Funcion Principal
esCapicua :: Integer -> Bool
esCapicua n | n < 10 = True
            | cantDigitos(n) == 2 = iesimoDigito n 1 == iesimoDigito n 2
            | otherwise = iesimoDigito n 1 == ultimoDigito && esCapicua sacarPrimeryUltimoDigito
            where ultimoDigito = mod n 10
                  sacarPrimeryUltimoDigito = sacarPrimerDigito (sacarUltimoDigito n) 


-- EJERCICIO 14--------
{-- ESPECIFICACION:
		problema sumaPotencias (q:N, n:N, m:N):N{
    requiere: {n >= a >=1}
    requiere: {m >= b >=1}
    asegura: {resultado = doble sumatoria (q^(n+m))}
--}
--Funcion Auxiliar
sumatoria :: Integer -> Integer -> Integer -> Integer
sumatoria q n m| n == 1 = q ^ (n+m)
               | otherwise = sumatoria q (n-1)  m + (q ^ (n+m))
               
--Funcion Principal
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m | m == 1 = sumatoria q n m
                    | otherwise = sumaPotencias q n (m-1)  + sumatoria q n m


-- EJERCICIO 16--------
menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n m | n == m = n
                      | mod n m == 0 = m
                      | otherwise = menorDivisorDesde n (m+1)

esPrimo :: Integer -> Bool
esPrimo 1 = False   
esPrimo n = not(menorDivisor n < n && menorDivisor n > 1) 

{-sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | -}

nEsimoPrimo :: Integer ->Integer
nEsimoPrimo n = nEsimoPrimoDesde n 1

nEsimoPrimoDesde :: Integer -> Integer -> Integer
nEsimoPrimoDesde n m | n == 0 = m - 1
                     | esPrimo m = nEsimoPrimoDesde (n-1) (m+1)
                     | otherwise = nEsimoPrimoDesde n (m+1)