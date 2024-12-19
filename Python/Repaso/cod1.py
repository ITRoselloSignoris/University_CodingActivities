from typing import List,Tuple


"""EJERCICIO 1"""

#cantidadproductovendida = 0

#tupla = ("empleado","product",cantidadproductovendida)
#list_tuplas = [(tupla)]

# nombreEmpleado:str = input("¿Cuál es el nombre del empleado?")
# nombreProducto:str = input("¿Cuál es el nombre del producto?")
# cantidadVendidaProducto:int = input("¿Cuántas unidades se vendieron de ese producto")

#dic = {
#    nombreEmpleado = [("nombreProducto", cantidadVendidaProducto),.......]
#}


lista_tuplas1:List = [("empleado1","product1", 5),("empleado1","product2", 3),("empleado2","product1", 1),("empleado2","product3", 10)]
lista_tuplas2:List = [("empleado1","product1", 5)]
lista_tuplas3:List = []


def crear_diccionario(ventasEmpleadoProducto:List)->dict:
    dic:dict ={}
    ls:List = []
    for elem in range(0,len(ventasEmpleadoProducto)):
        ls.append(ventasEmpleadoProducto[elem][0])
    for elem in ls:
        dic[elem] = []

    return dic

def gestion_ventas(ventasEmpleadoProducto:List)-> dict:

    dic:dict = crear_diccionario(ventasEmpleadoProducto)
    for empleado, producto, cantidad in ventasEmpleadoProducto:
        dic[empleado].append((producto,cantidad))

    return dic




print(gestion_ventas(lista_tuplas1))
print(gestion_ventas(lista_tuplas2))
print(gestion_ventas(lista_tuplas3))


"""EJERCICIO 2"""
numeros1:List = [57, 2383, 812, 246]
numeros2:List = [1256]
numeros3:List = [1]
numeros4:List = []


def cantidad_digitos_impares(numeros:List)->int:
    contador:int = 0
    for num in numeros:
        for digit in str(num):
            if int(digit) % 2 != 0:
                contador += 1
    
    return contador

print(cantidad_digitos_impares(numeros1))
print(cantidad_digitos_impares(numeros2))
print(cantidad_digitos_impares(numeros3))
print(cantidad_digitos_impares(numeros4))



"""EJERCICIO 3"""

from queue import Queue as Cola

#carpeta = ("id_carpeta",numero_de_paginas)
#   ESCASAS ULTIMOAS <  /UMBRAL/ < NUMEROSAS PRIMERO

carpeta1:tuple = ("id1", 10)
carpeta2 = ("id2", 11)
carpeta3 = ("id3",5)
carpeta4 = ("id4",20)
carpeta5 = ("id5",6)

carpetas:Cola = Cola()
carpetas.put(carpeta1)
carpetas.put(carpeta2)
carpetas.put(carpeta3)
carpetas.put(carpeta4)
carpetas.put(carpeta5)

def reordenar_cola_primero_numerosas(carpetas:Cola, umbral:int)-> Cola:

    colaMenosUmbral = Cola()
    colaMasUmbral = Cola()

    while carpetas.empty() == False:
        elem = carpetas.get()
        if elem[1] > umbral:
            colaMasUmbral.put(elem)
        else:
            colaMenosUmbral.put(elem)

    while colaMasUmbral.empty() == False:
        elem = colaMasUmbral.get()
        carpetas.put(elem)
        
    while colaMenosUmbral.empty() == False:
        elem = colaMenosUmbral.get()
        carpetas.put(elem)

    while carpetas.empty() == False:
        elem = carpetas.get()
        print(elem)

    return carpetas
        

reordenar_cola_primero_numerosas(carpetas, 10)

"""EJERCICIO 4"""
matriz1 = [
    [3,6,9,12],
    [2,5,8,11],
    [1,4,7,10]]
matriz2 = [
    [4,3,2,1],
    [4,3,2,1],
    [4,3,2,1]]
matriz3 = [
    [9, 8, 7],
    [10, 6, 4],
    [3, 2, 1]
]


def matriz_cuasi_decreciente(matriz:List) -> bool:

    MatrizconValoresPorColumnas:List = []
    maxPorColumna:List = []

    for x in range(0,len(matriz[0])):
        MatrizconValoresPorColumnas.append([])
        for fila in matriz:
            MatrizconValoresPorColumnas[x].append(fila[x])

    for columna in MatrizconValoresPorColumnas:
        maximo:int = 0
        for val_column in columna:
            if val_column >= maximo:
                maximo:int = val_column
        maxPorColumna.append(maximo)

    maximo:str = ""
    for valor in maxPorColumna:
        if maximo == "" or valor < maximo:
            maximo:int = valor
        else:
            return False

    return True

print(matriz_cuasi_decreciente(matriz1))
print(matriz_cuasi_decreciente(matriz2))
print(matriz_cuasi_decreciente(matriz3))




"""
TEORIA

A. 3ERA
B. 2DA
C. 3ERA

"""