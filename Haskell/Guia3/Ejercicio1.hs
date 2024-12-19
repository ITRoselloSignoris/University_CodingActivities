double :: Integer -> Integer
double x = x + x

squareroot :: Integer -> Integer
squareroot y = y * y

f :: Integer -> Integer 
f z | z == 1 = 8
    | z == 4 = 131
    | z == 16 = 16
    | otherwise = undefined

g :: Integer -> Integer
g n | n == 16 = 4
    | n == 131 = 1
    | n == 8 = 16
    | otherwise = undefined

h :: Integer -> Integer
h n = f (g n)


k :: Integer -> Integer
k n = g (f n)