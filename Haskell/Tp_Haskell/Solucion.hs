module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (x:xs) | vueloRepetidoenAgenciaDeViajes x xs || not(vueloValido (x:xs)) = False
                     | otherwise = vuelosValidos xs

-- Verifica si hay un vuelo que se repite por lo menos una vez en AgenciaDeViajes
vueloRepetidoenAgenciaDeViajes :: Vuelo -> AgenciaDeViajes -> Bool
vueloRepetidoenAgenciaDeViajes _ [] = False
vueloRepetidoenAgenciaDeViajes (c1, c2, d) ((x, y, z):xs) | c1 == x && c2 == y = True
                                     | otherwise = vueloRepetidoenAgenciaDeViajes (c1, c2, d) xs

vueloValido :: [Vuelo] -> Bool
vueloValido [] = True
vueloValido ((origen, destino, duracion): vs) | origen == destino || duracion <= 0 = False
                                              | otherwise = vueloValido vs

-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas agencia ciudad = eliminarRepetidos (armarListaCiudadesConectadas agencia ciudad)

-- Devuelve una lista con las ciudades conectadas con la ciudad que le pasemos
armarListaCiudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
armarListaCiudadesConectadas [] _ = []
armarListaCiudadesConectadas ((c1,c2,d):xs) n | n == c1 = c2:armarListaCiudadesConectadas xs n
                                              | n == c2 = c1:armarListaCiudadesConectadas xs n
                                              | otherwise = armarListaCiudadesConectadas xs n
-- Devuelve una lista sin repetidos
eliminarRepetidos :: [Ciudad] -> [Ciudad]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | x `elem` xs = eliminarRepetidos xs
                         | otherwise = x:eliminarRepetidos xs

-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((c1, c2, d):xs) = (c1, c2, d - (d/10.0)): modernizarFlota xs

-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada n = buscarCiudadMasConectada "" n n

buscarCiudadMasConectada :: Ciudad -> AgenciaDeViajes -> AgenciaDeViajes -> Ciudad
buscarCiudadMasConectada n _ [] = n
buscarCiudadMasConectada n lista ((c1, c2, d):xs)| cantidadConexionesc2ac1 c2 lista > cantidadConexionesc2ac1 n lista = buscarCiudadMasConectada c2 lista xs
                                                 | otherwise = buscarCiudadMasConectada n lista xs

-- Devuelve la cantidad de conexiones de una ciudad destino (c2) que le pasemos con ciudades de origen (c1)
cantidadConexionesc2ac1 :: Ciudad -> AgenciaDeViajes -> Integer 
cantidadConexionesc2ac1 _ [] = 0
cantidadConexionesc2ac1 ciudad ((c1,c2,d):xs) | ciudad == c1 = 1 + cantidadConexionesc2ac1 ciudad xs
                                              | otherwise = cantidadConexionesc2ac1 ciudad xs                             
-- EJERCICIO 5 
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar agencia origen destino =  existeVueloDirecto agencia origen destino || existeEscala agencia (ciudadesIntermedias agencia origen) destino
-- Verifica si hay un vuelo directo entre las ciudades o si hay un vuelo con escalas desde el origen hasta el destino

existeVueloDirecto :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool -- Verifica si hay un vuelo directo entre dos ciudades
existeVueloDirecto [] _ _ = False -- Si no hay vuelos en la agencia, no hay conexión
existeVueloDirecto ((c1, c2, _):xs) origen destino = (origen == c1 && destino == c2) || existeVueloDirecto xs origen destino 
-- Comprueba si el vuelo conecta origen y destino directamente o sigue buscando en los vuelos restantes


-- Verifica si existe una ruta con escalas para llegar al destino
existeEscala :: AgenciaDeViajes -> [Ciudad] -> Ciudad -> Bool
existeEscala _ [] _ = False -- Si no hay ciudades intermedias, no hay conexión
existeEscala agencia (c1:xs) destino = existeVueloDirecto agencia c1 destino || existeEscala agencia xs destino  
-- Comprueba si hay un vuelo directo desde una ciudad intermedia al destino o sigue buscando desde otras ciudades intermedias

-- Encuentra las ciudades intermedias alcanzables desde una ciudad origen
ciudadesIntermedias :: AgenciaDeViajes -> Ciudad -> [Ciudad] 
ciudadesIntermedias [] _ = [] -- Si no hay vuelos en la agencia, no hay ciudades intermedias
ciudadesIntermedias ((c1, c2, _):xs) origen | origen == c1 = c2 : ciudadesIntermedias xs origen -- Si el vuelo parte del origen, agrega la ciudad destino
                                         | otherwise = ciudadesIntermedias xs origen --Sino sigue buscando

-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido agencia origen destino = seleccionaMinimo (vuelosDirectos agencia origen destino ++ vuelosConEscala agencia origen destino) 
-- Combina las duraciones de los vuelos directos y con escalas, y selecciona la mínima

vuelosDirectos :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion] -- Encuentra las duraciones de los vuelos directos
vuelosDirectos [] _ _ = [] -- Si no hay vuelos, no hay duraciones
vuelosDirectos ((c1, c2, c3):xs) origen destino  
                                                 | c1 == origen && c2 == destino = c3 : vuelosDirectos xs origen destino  
                                                 -- Si el vuelo conecta origen y destino, agrega su duración
                                                 | otherwise = vuelosDirectos xs origen destino -- Sino sigue buscando

-- Encuentra las duraciones de los vuelos con una escala
vuelosConEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion]
vuelosConEscala [] _ _ = [] -- Si no hay vuelos, no hay rutas con escalas
vuelosConEscala ((x1, y1, z1):xs) origen destino
                                                  | x1 == origen = buscarDesdeDestino y1 z1 xs destino ++ vuelosConEscala xs origen destino 
                                                  -- Busca las duraciones de los vuelos que parten de la ciudad origen y hacen escala
                                                  | otherwise = vuelosConEscala xs origen destino --Sino sigue buscando 

-- Busca las duraciones de vuelos con escala (origen -> escala -> destino)
buscarDesdeDestino :: Ciudad -> Duracion -> AgenciaDeViajes -> Ciudad -> [Duracion]
buscarDesdeDestino _ _ [] _ = [] -- Si no hay mas vuelos, no hay rutas
buscarDesdeDestino y1 z1 ((x2, y2, z2):xs) destino 
                                                   | y1 == x2 && y2 == destino = (z1 + z2) : buscarDesdeDestino y1 z1 xs destino  
                                                   -- Si el vuelo conecta la escala con el destino, calcula la duración total
                                                   | otherwise = buscarDesdeDestino y1 z1 xs destino --Sino sigue buscandio

seleccionaMinimo :: [Duracion] -> Duracion
seleccionaMinimo [] = 0
seleccionaMinimo [x] = x
seleccionaMinimo (c1:c2:xs) | c1 > c2 = (seleccionaMinimo (c2:xs))
                            | otherwise = seleccionaMinimo (c1:xs)
-- Ejercicio 7                            
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen vuelos origen = existeRuta origen origen vuelos []  
-- Busca una ruta desde la ciudad de origen hasta sí misma, empezando con una lista vacía de ciudades visitadas.

-- Función recursiva que verifica si existe una ruta desde 'origen' a 'destino'
existeRuta :: Ciudad -> Ciudad -> AgenciaDeViajes -> [Ciudad] -> Bool
existeRuta origen destino vuelos visitadas
  | origen == destino && length visitadas > 1 = True  
  -- Si el origen es igual al destino y ya hemos visitado más de una ciudad, existe una ruta
  | otherwise = buscarRuta vuelos visitadas origen destino  
  -- Si no hemos llegado al destino, busca rutas recursivamente con la función 'buscarRuta'

-- Función que busca una ruta en la lista de vuelos
buscarRuta :: AgenciaDeViajes -> [Ciudad] -> Ciudad -> Ciudad -> Bool
buscarRuta [] _ _ _ = False  -- Si no hay vuelos disponibles, no existe la ruta.
buscarRuta ((ciudad1, ciudad2, _):vs) visitadas actual destino
  | ciudad1 == actual && ciudad2 `notElem` visitadas = existeRuta ciudad2 destino vs (ciudad2 : visitadas)
  -- Si el vuelo va desde la ciudad actual y no hemos visitado aún la ciudad de destino (ciudad2)
  -- entonces buscamos si desde ciudad2 se puede llegar a destino, añadiéndola a la lista de visitadas
  | otherwise = buscarRuta vs visitadas actual destino  
  -- Sino encontramos un vuelo válido, seguimos buscando en los vuelos restantes
