#Class to 
#
#
#Variables:
#
#
NERFromStringBufferPipe <- R6Class(
  
  "NERFromStringBufferPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[NERFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
    
    pipe = function(instance) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[NERFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
            
      return(instance)
    }
  )
)
