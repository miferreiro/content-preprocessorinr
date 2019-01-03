#Class to 
#
#
#Variables:
#
#
AbbreviationFromStringBufferPipe <- R6Class(
  
  "AbbreviationFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[AbbreviationFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
    
    pipe = function(instance) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[AbbreviationFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      return(instance)
    }
  )
)
