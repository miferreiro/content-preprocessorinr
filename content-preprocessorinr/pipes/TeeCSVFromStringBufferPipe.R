TeeCSVFromStringBufferPipe <- R6Class(
  
  "TeeCSVFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = ""){
      
      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
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
      
      row <- list()

      path <- instance$getPath()
      source <- instance$getSource()
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

      # write.table(rbindlist(list(row)), fileName, append = T, col.names = !file.exists(fileName),sep = ";", row.names = FALSE,qmethod  = c("double"))

      dataFrame <<- rbind(dataFrame, rbind(row),make.row.names = F)

      return(instance)
    }
  )
)

