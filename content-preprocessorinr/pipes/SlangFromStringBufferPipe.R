#Class to 
#
#
#Variables:
#
#
SlangFromStringBufferPipe <- R6Class(
  
  "SlangFromStringBufferPipe",
  
  public = list(
    
    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[SlangFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
    
    pipe = function(instance) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[SlangFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      instance$getData() %>>% 
        self$replaceSlang() %>>% 
          instance$setData()
    },
    
    replaceSlang = function(data) {
      return(data %>>% replace_internet_slang())
    }
  )
)
