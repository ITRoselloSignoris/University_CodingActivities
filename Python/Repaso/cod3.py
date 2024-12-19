from queue import Queue as Cola
##EJ1




##EJ2
def reordenar_cola_priorizando_vips(filaClientes:Cola[tuple[str,str]])->Cola[str]:
    vips=[]
    comunes=[]
    while not(filaClientes.empty()):
        cliente=filaClientes.get()
        if cliente[1] == "comun":
            comunes.append(cliente)
        else:
            vips.append(cliente)
    for vip in vips:
        filaClientes.put(vip)
    for comun in comunes:
        filaClientes.put(comun)
    return filaClientes

##EJ3



##EJ4
def hace_tateti(jugador:str,table:list[list[str]])->bool:
    for col in range(len(table[0])):
        contador=0
        for fila in range(len(table)):
            if table[fila][col] == jugador:
                contador += 1
        if contador == 3:
            return True
    return False

def quien_gano_el_tateti_facilito(table:list[list[str]])-> int:
    res=0
    tateti_ana = hace_tateti("X",table)
    tateti_beto = hace_tateti("O",table)
    if tateti_ana and tateti_beto:
        res = 3
    elif tateti_ana and not(tateti_beto):
        res = 1
    elif not(tateti_ana) and tateti_beto:
        res = 2
    else:
        res=0
    return res

"""
##Pertenece

def pertenece(lista:list, palabra:str)->bool:
    response = False
    for elem in lista:
        if elem == palabra:
            response = True 
    return response

##Texto a lista de strings(palabras)
def text_to_list(texto:str)->list:
    ls=[]
    for caracter in texto:
        palabra=""
        if caracter == " ":
            ls.append(palabra)
        else:
            palabra += caracter
    return ls


##Contador según condición
def condicion(elem:str)->int: ##Contar vocales de la palabra
    vocales = ["aeiouAEIOU"]
    for caracter in elem:
        contador += int(pertenece(vocales,caracter))


def contador(lista:list)->int:
    contador=0
    for elem in lista:
        contador = int(condicion(elem))
        if contador != 0 and pertenece():


##"""