#Class that implements the flow of pipes. This class inherits from the TypePipe 
# class, which has the pipeAll method that has a default implementation.
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
      #Function that implements the pipeAll function of the super class.
      #In this case, the pipeAll function is called so that the default pipe 
      #stream is executed
      #Args: 
      #   instance: (Instance) The instance that is going to be processed
      #
      #Returns: 
      #   The preprocessed instance
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[SerialPipes][pipeAll][Error] 
                Checking the type of the variable: instance ", 
                  class(instance));
      }
      
      cat("[SerialPipes][pipeAll][Info] ", instance$getPath(), "\n")
      
      instance <- super$pipeAll(instance)
      
      return(instance)
    }
  )
)