StringBuffer2SynsetVectorPipe <- R6Class(
  
  "StringBuffer2SynsetVectorPipe",
  
  public = list(
        
    pipe = function(instancia) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error]
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
