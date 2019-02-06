#Class to 
#
#
#Variables:
#

SerialPipes <- R6Class(
  
  "SerialPipes",
  
  inherit = TypePipe,
  
  public = list(
    
    initialize = function() {
      
    },
    
    pipeAll = function(instance) {
    
      #
      #
      #Args: 
      #   
      #
      #Returns: 
      #   
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[SerialPipes][pipeAll][Error] 
             Checking the type of the variable: instance ", 
             class(instance));
      }
      print(instance$getPath())
      instance <- super$pipeAll(instance)
      
      return(instance)
    }
  )
)