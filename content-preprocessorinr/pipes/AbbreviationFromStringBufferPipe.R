AbbreviationFromStringBufferPipe <- R6Class(
  
  "AbbreviationFromStringBufferPipe",
  
  public = list(
      
    pipe = function(instancia) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[AbbreviationFromStringBufferPipe][pipe][Error]
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
