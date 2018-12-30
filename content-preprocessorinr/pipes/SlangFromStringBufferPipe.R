
SlangFromStringBufferPipe <- R6Class(
  
  "SlangFromStringBufferPipe",
  
  public = list(
        
    pipe = function(instancia) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[SlangFromStringBufferPipe][pipe][Error]
             Checking the type of the variable: instance ", class(instance))
      }
      
      instancia$getData() %>>% 
        self$replaceSlang() %>>% 
            instancia$setData()
    },
    
    replaceSlang = function(data) {
      return(data %>>% replace_internet_slang())
    },
    
    getPropertyName = function() {
      return(private$propertyName)
    }
  ),  
  private = list(
    propertyName = "Slang"
  )
)
