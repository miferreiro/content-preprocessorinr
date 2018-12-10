
{ 
    #Carga todas las librerias y crea la lista de instancia inicial
    #Construye el source y las propiedad de todos los objetos
    #Construye una lista de booleanos, donde TRUE es la posicion de la lista listaIntacias donde es v�lida, 
    #FALSE, si no lo es    
    #Obtenemos la lista de instancais validas
    #Se aplica la funcion de obtener propiedades Iniciales a las instancias validas
    
    arcTwtid <- list.files(path = archivosTest,
                           pattern = "twtid"
                           ,recursive = TRUE
                           ,full.names = TRUE
                           ,all.files = TRUE)

    listaInstanciasTwtid <- list();
    
    listaInstanciasTwtid <- sapply(arcTwtid, DataSource$public_methods$createInstance)

    invisible(sapply(listaInstanciasTwtid,propiedadesTextoDate))


}
