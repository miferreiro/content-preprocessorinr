
{ 
#Carga todas las librerias y crea la lista de instancia inicial
#Construye el source y las propiedad de todos los objetos
#Construye una lista de booleanos, donde TRUE es la posicion de la lista listaIntacias donde es v�lida, 
#FALSE, si no lo es    
#Obtenemos la lista de instancais validas
#Se aplica la funcion de obtener propiedades Iniciales a las instancias validas

rm(list = ls()) 
archivosTest = "content-preprocessorinr/testFiles/tests";
patternLista = "tsms"
source("content-preprocessorinr/inicializacion.R")

invisible(sapply(listaInstancias,propiedadesTextoDate))

invalid <- lapply(listaInstancias,deleteInvalidInstances)
listaInstanciasValidas <- obtainValidInstances()

invisible(sapply(listaInstanciasValidas,propiedadesIniciales))

#View(listaInstanciasValidas)
}
{
invisible(sapply(listaInstanciasValidas,pipes))

#Muestra las propiedades
for (x in listaInstanciasValidas) {
    print(x$getSource())
    print("---------------------------------------------------")
    print(x$getData())
    print("|||||||||||||||||||||||||||||||||||||||||||||||||||")
}

#Hacer csv
#fun$toCsv(listaInstanciasValidas)
}
