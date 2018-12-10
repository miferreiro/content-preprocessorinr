
{ 
    #Carga todas las librerias y crea la lista de instancia inicial
    #Construye el source y las propiedad de todos los objetos
    #Construye una lista de booleanos, donde TRUE es la posicion de la lista listaIntacias donde es v�lida, 
    #FALSE, si no lo es    
    #Obtenemos la lista de instancais validas
    #Se aplica la funcion de obtener propiedades Iniciales a las instancias validas
    
    arcYtbid <- list.files(path = archivosTest
                           ,pattern="ytbid"
                           ,recursive = TRUE
                           ,full.names = TRUE
                           ,all.files = TRUE)
    
    listaInstanciasYtbid <- list();

    listaInstanciasYtbid <- sapply(arcYtbid, DataSource$public_methods$createInstance)

    invisible(sapply(listaInstanciasYtbid,propiedadesTextoDate))

}

