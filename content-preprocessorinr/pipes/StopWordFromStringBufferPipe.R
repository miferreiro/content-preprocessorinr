StopWordFromStringBufferPipe <- R6Class(
  
  "StopWordFromStringBufferPipe",
  
  public = list(
        
    pipe = function(instancia) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[StopWordFromStringBufferPipe][pipe][Error]
             Checking the type of the variable: instance ", class(instance))
      }
        
      instancia$getData() %>>% 
          self$stopWords() %>>%
              {instancia$setData(.)}
        
      return(instancia)
    },
    
    stopWords = function(data) {
      return(data )
    },
    
    getPropertyName = function() {
      return(private$propertyName)
    }
    
  ),  
  private = list(
    propertyName = ""
  )
)
