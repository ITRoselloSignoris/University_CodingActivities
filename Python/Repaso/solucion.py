##Ejercicios 2do Parcial - MI tema
from queue import Queue as Cola

##EJ1
def subsecuencia_mas_larga(v:list[int])->tuple[int,int]:
    res:tuple[int,int] = (1,0)
    for desde in range(len(v)):
        for hasta in range(desde+1,len(v)):
            if (v[hasta] - v[hasta-1]) != 1 and (v[hasta-1] - v[hasta]) != 1:
                break
            longitud = hasta-desde + 1
            if longitud > res[0]:
                res = (longitud, desde)
    return res
##EJ2
def contador_posibles_correctas(lista)->int:
    contador=0
    verdaderos=0
    for x in lista:
        verdaderos += int(x)
    if verdaderos == (len(lista)/2):
        contador= len(lista)
    elif verdaderos < (len(lista)/2):
        contador = int(len(lista)/2) + verdaderos
    else:
        falsos = len(lista) - verdaderos
        contador = int(len(lista)/2) + falsos
    return contador

def mejor_resultado_de_ana(examenes:Cola[list[bool]]) -> list[int]:
    ls=[]
    while not(examenes.empty()):
        elem=examenes.get()
        posibles_correctas = contador_posibles_correctas(elem)
        ls.append(posibles_correctas)
    print(ls)
    return ls
##EJ3
##COMprobar A antes y despues
A=[[1,2,3],
   [4,5,6],
   [7,8,9]]

print(A)

def cambiar_matriz(A:list[list[int]]) -> None:
    matriz_copia1=A.copy()
    matriz_copia=[]
    for fila in A:
        matriz_copia.append([])
    for fila in range(len(A)):
        for col in range(0,len(A[fila])):
            matriz_copia[fila].append(A[fila][col-1])
    for fila in range(len(A)):
        for col in range(0,len(A[fila])):
            matriz_copia1[fila][col] = matriz_copia[fila][col]

cambiar_matriz(A)
print(A)
##EJ4
def pertenece(string:str,lista:list)->bool:
    response=False
    for x in lista:
        if string == x:
            response=True
    return response

def contador_vocales(palabra:str)->int:
    contador=0
    vocales:str = "aeiouAEIOU"
    for caract in palabra:
        contador += int(pertenece(caract,vocales))
    return contador

def palabras_por_vocales(texto:str)->dict[int,int]:
    palabras:list=[]
    palabra:str=""
    texto += " "
    contador:int=0
    for caracter in texto:
        if caracter == " ":
            if len(palabra) != 0:
                palabras.append(palabra)
                palabra:str=""
        else:
            palabra += caracter
    dic:dict[int,int] = {}
    for pal in palabras:
        cantidadVocales:int=contador_vocales(pal)
        if pertenece(cantidadVocales,dic.keys()):
            dic[cantidadVocales] += 1
        else:
            dic[cantidadVocales] = 1
    return dic