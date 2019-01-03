#Class to 
#
#
#Variables:
#
#
StopWordFromStringBufferPipe <- R6Class(
  
  "StopWordFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StopWordFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
        
    pipe = function(instance) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[StopWordFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      instance$getData() %>>% 
        self$stopWords() %>>%
          {instance$setData(.)}
        
      return(instance)
    },
    
    stopWords = function(data) {
      return(data )
    }
  )
)
