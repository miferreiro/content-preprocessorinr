proccess_files = function(pathFiles, 
                          pipe, 
                          pathKeys = "content-preprocessorinr/config/configurations.ini", 
                          pathOutPut = "output.RData") {
  
  if (!"character" %in% class(pathFiles)) {
    stop("[proccess_files][Error] 
         Checking the type of the variable: pathFiles ", 
         class(pathFiles))
  } 
  
  if (!"TypePipe" %in% class(pipe)) {
    stop("[proccess_files][Error] 
         Checking the type of the variable: pipe ", 
         class(pipe))
  } 
  
  if (!"character" %in% class(pathOutPut)) {
    stop("[proccess_files][Error] 
         Checking the type of the variable: pathOutPut ", 
         class(pathOutPut))
  }  
  
  if (!file_ext(pathOutPut) %in% "csv" & !file_ext(pathOutPut) %in% "RData") {
    stop("[proccess_files][Error] 
         Checking the extension of  pathOutPut")
  }
  
  Files <- list.files(path = pathFiles, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  
  #Inicializamos el objeto que manejar. los diferentes tipos de cosnexiones: youtube y twitter
  connections <<- Connections$new(pathKeys)
  resourceHandle <<- ResourceHandler$new()
  dataFrame <<- data.frame()
  
  #Creamos la lista de instancias, las cuales contendran el date, source, path,data y una lista de propiedades
  #del archivo que se encuentra en el path indicado
  InstancesList <- sapply(Files, FactoryMethod$new()$createInstance)
  cat("Se han creado: ",length(InstancesList)," instancias.\n")
  InstancesList <- sapply(InstancesList, pipe$pipeAll)
  
  
  if (file_ext(pathOutPut) %in% "csv") {
    
    write.table(x = matrix(unlist(dataFrame),
                           nrow = dim(dataFrame)[1], 
                           ncol = dim(dataFrame)[2], 
                           dimnames = list(NULL, names(dataFrame))), 
                file = pathOutPut, sep = ";",col.names = TRUE,row.names = FALSE)
  } else {
    
    if (file_ext(pathOutPut) %in% "RData") {
      saveRDS(get("dataFrame"), file = pathOutPut)
    }
  }
  
  return(dataFrame)
}