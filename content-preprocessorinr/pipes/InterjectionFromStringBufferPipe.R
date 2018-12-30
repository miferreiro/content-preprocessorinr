InterjectionFromStringBufferPipe <- R6Class(
  
  "InterjectionFromStringBufferPipe",
  
  public = list(
      
    pipe = function(instancia) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[InterjectionFromStringBufferPipe][pipe][Error]
             Checking the type of the variable: instance ", class(instance))
      }
        
      return(instancia)
    },
    
    getPropertyName = function() {
      return(private$propertyName)
    }
  ),  
  
  private = list(
      propertyName = ""
  )
)
