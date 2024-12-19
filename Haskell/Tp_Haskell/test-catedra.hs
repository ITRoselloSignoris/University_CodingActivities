import Test.HUnit
import Data.List
import Solucion
-- No está permitido agregar nuevos imports.


runCatedraTests = runTestTT allTests

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

-- corregir los tests si es necesario con las funciones extras que se encuentran al final del archivo

testsEjvuelosValidos = test [
    "vuelos valido sin ningun elemento" ~: vuelosValidos [] ~?= True,
    "vuelos valido con un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True ,
    "vuelos valido con mas de un elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 4.5)] ~?= True ,
    "vuelos no valido, vuelos repetidos" ~: vuelosValidos [("BsAs", "Rosario", 5.0), ("BsAs", "Rosario", 5.0)] ~?= False ,
    "vuelos no valido, vuelos iguales con distinta duracion" ~: vuelosValidos [("BsAs", "Rosario", 5.0), ("BsAs", "Rosario", 2.0)] ~?= False ,
    "vuelos no valido, contiene un vuelo con origen = destino" ~: vuelosValidos [("BsAs", "Rosario", 5.0), ("BsAs", "BsAs", 2.0)] ~?= False ,
    "vuelos no valido, contiene un vuelo con duracion = 0" ~: vuelosValidos [("BsAs", "Rosario", 0)] ~?= False
    ]

testsEjciudadesConectadas = test [
    "agencia vacia" ~: ciudadesConectadas [] "Rosario" ~?= [],
    "ciudad no conectada" ~: ciudadesConectadas [("BsAs", "Trelew", 5.0)] "Rosario" ~?= [],
    "ciudad conectada con un elemento" ~: ciudadesConectadas [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"] ,
    "ciudad conectada 'desde' con varios elementos" ~: expectAny (ciudadesConectadas [("Rosario", "BsAs", 5.0), ("BsAs", "Cordoba", 5.0), ("Rosario", "Cordoba", 5.0)] "Rosario") [["BsAs", "Cordoba"], ["Cordoba", "BsAs"]],
    "ciudad conectada 'hasta' con varios elementos" ~: expectAny (ciudadesConectadas [("BsAs", "Rosario", 5.0), ("BsAs", "Cordoba", 5.0), ("Cordoba", "Rosario", 5.0)] "Rosario") [["BsAs", "Cordoba"], ["Cordoba", "BsAs"]]
    ]

testsEjmodernizarFlota = test [
    "agencia vacia" ~: modernizarFlota [] ~?= [] ,
    "flota modernizada con un elemento" ~: modernizarFlota [("BsAs", "Rosario", 10.0)] ~?= [("BsAs", "Rosario", 9.0)] ,
    "flota modernizada con varios elementos" ~: expectPermutacion (modernizarFlota [("BsAs", "Rosario", 10.0), ("BsAs", "Cordoba", 5.0), ("Cordoba", "Rosario", 3.0), ("Rosario", "BsAs", 9.0)]) [("BsAs", "Rosario", 9.0), ("Cordoba", "Rosario", 2.7), ("BsAs", "Cordoba", 4.5), ("Rosario", "BsAs", 8.1)]
    ]

testsEjciudadMasConectada = test [
    "ciudad mas conectada con un solo vuelo" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0)] ~?= "",
    "ciudad mas conectada que aparece dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Cordoba", 7.0)] ~?= "Rosario" ,
    "ciudad mas conectada que aparece mas de dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("BsAs", "Cordoba", 5.0), ("Cordoba", "Rosario", 3.0), ("Trelew", "BsAs", 9.0)] ~?= "BsAs" ,
    "ciudad mas conectada que es una de las que aparecen mas veces" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0), ("BsAs", "Cordoba", 5.0), ("Cordoba", "Rosario", 3.0), ("Rosario", "BsAs", 9.0)]) ["BsAs", "Rosario"]
    ]

testsEjsePuedeLlegar = test [
    "Se puede llegar caso falso porque la agencia esta vacia" ~: sePuedeLlegar [] "BsAs" "Cordoba" ~?= False ,
    "Se puede llegar caso verdadero con una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" "Cordoba" ~?= True ,
    "Se puede llegar caso verdadero directo" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" "Rosario" ~?= True ,
    "Se puede llegar caso falso porque no hay vuelos de vuelta" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "Trelew", 8.0)] "Cordoba" "BsAs" ~?= False ,
    "Se puede llegar caso falso porque no hay vuelos" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" "Trelew" ~?= False ,
    "Se puede llegar caso falso porque hace falta mas de una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" "BsAs" ~?= False
    ]

testsEjduracionDelCaminoMasRapido = test [
    "duración del camino más rápido con una escala" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" "Cordoba" ~?= 10.0 ,
    "duración del camino más rápido directo" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" "Rosario" ~?= 5.0 ,
    "duración del camino más rápido directo con opcion de escala" ~: duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("BsAs", "Cordoba", 8.0), ("Cordoba", "BsAs", 7.4)] "BsAs" "Cordoba" ~?= 8.0 ,
    "duración del camino más rápido con escala más rapida" ~: duracionDelCaminoMasRapido [("BsAs", "Lima", 10.0), ("Lima", "Cordoba", 8.0), ("BsAs", "Trelew", 8.0), ("Trelew", "Cordoba", 7.4)] "BsAs" "Cordoba" ~?= 15.4 ,
    "duracion del camino más rápido con escala más rapida que directo" ~: duracionDelCaminoMasRapido [("BsAs", "Cordoba", 10.0), ("Lima", "Cordoba", 8.0), ("BsAs", "Trelew", 1.0), ("Trelew", "Cordoba", 2.4)] "BsAs" "Cordoba" ~?= 3.4 
    ]

testsEjpuedoVolverAOrigen = test [
    "puedo volver a origen caso falso porque la agencia esta vacia" ~: puedoVolverAOrigen [] "Trelew" ~?= False ,
    "puedo volver a origen caso verdadero con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" ~?= True ,
    "puedo volver a origen caso verdadero en otro orden" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "BsAs", 8.0)] "Cordoba" ~?= True ,
    "puedo volver a origen caso verdadero directo" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 5.0), ("Cordoba", "BsAs", 8.0)] "BsAs" ~?= True ,
    "puedo volver a origen caso verdadero varias escalas" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Cordoba", 5.0), ("Cordoba", "Trelew", 8.0), ("Trelew", "BsAs", 6.9)] "BsAs" ~?= True ,
    "puedo volver a origen caso falso porque no hay vuelos" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 5.0), ("Cordoba", "BsAs", 8.0)] "Trelew" ~?= False ,
    "puedo volver a origen caso falso porque no conectan" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 5.0), ("Cordoba", "BsAs", 8.0)] "Cordoba" ~?= False 
    ]



-- Funciones extras

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)