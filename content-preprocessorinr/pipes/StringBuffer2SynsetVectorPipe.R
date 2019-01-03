#Class to 
#
#
#Variables:
#
#
StringBuffer2SynsetVectorPipe <- R6Class(
  
  "StringBuffer2SynsetVectorPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },  
    
    pipe = function(instance) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      return(instance)
    }
  )
)
