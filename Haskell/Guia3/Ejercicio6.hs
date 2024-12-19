type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto x | mod x 100 == 0 && mod x 400 /= 0 = False
           | otherwise = esMultiploDe x 4 