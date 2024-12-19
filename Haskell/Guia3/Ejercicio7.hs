

absoluto2 :: Float -> Float 
absoluto2 n | n < 0 = -n
            | otherwise = n

distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x1, y1, z1) (x2, y2, z2) = absoluto2(x1-x2) + absoluto2(y1-y2) + absoluto2(z1-z2)