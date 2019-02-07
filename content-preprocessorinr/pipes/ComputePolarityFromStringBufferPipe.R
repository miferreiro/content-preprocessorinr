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
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[ComputePolarityFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[ComputePolarityFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
    }, 
        
    pipe = function(instance){
      
      if (!"Instance" %in% class(instance)) {
        stop("[ComputePolarityFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", class(instance))
      }
       
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "ComputePolarityFromStringBufferPipe")
      
      if (!super$checkCompatibility("ComputePolarityFromStringBufferPipe")) {
        stop("[ComputePolarityFromStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
       
      return(instance)
    }
  )
)
