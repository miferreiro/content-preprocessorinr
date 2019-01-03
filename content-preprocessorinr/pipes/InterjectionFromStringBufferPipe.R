#Class to 
#
#
#Variables:
#
#
InterjectionFromStringBufferPipe <- R6Class(
  
  "InterjectionFromStringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[InterjectionFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
    
    pipe = function(instance) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[InterjectionFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      return(instance)
    }
  )
)
