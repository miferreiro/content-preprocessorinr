#Class to 
#
#
#Variables:
#
#
ComputePolarityFromStringBufferPipe <- R6Class(
  
  "ComputePolarityFromStringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[ComputePolarityFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
        
    pipe = function(instance){
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[ComputePolarityFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", class(instance))
      }
        
      return(instance)
    }
  )
)
