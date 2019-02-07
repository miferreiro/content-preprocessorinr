TeeCSVFromStringBufferPipe <- R6Class(
  
  "TeeCSVFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TeeCSVFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TeeCSVFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance, fileName = "propiedades.csv", withData = TRUE) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"character" %in% class(fileName)) {
        stop("[TeeCSVFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: fileName ", 
                  class(fileName))
      }  
      
      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: withData ", 
                  class(withData))
      }
      
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "TeeCSVFromStringBufferPipe")
      
      if (!super$checkCompatibility("TeeCSVFromStringBufferPipe")) {
        stop("[TeeCSVFromStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      
      if (!instance$isInstanceValid()) {
        
        return(instance)
      
      }
      
      row <- list()

      path <- instance$getPath()
      source <- paste0(unlist(instance$getSource()))
      date <- instance$getDate()
      
      if (withData) {
        
        data <- instance$getData()
        
        row <- list.append(row, path, source, date, data)
        rowNames <- list("path", "source", "date","data")
        
      } else {
        row <- list.append(row, path, source, date)
        rowNames <- list("path", "source", "date")
      }
      
      for (name in instance$getNamesOfProperties()) { 
        rowNames <- list.append(rowNames, name)
      }
      
      for (property in instance$getProperties()) { 
        row <- list.append(row, paste0(unlist(property),collapse = "|"))
      }
      
      names(row) <- rowNames

      dataFrame <<- rbind(dataFrame, rbind(row),make.row.names = F)

      return(instance)
    }
  )
)

