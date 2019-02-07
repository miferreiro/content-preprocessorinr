#Class to 
#
#
#Variables:
#
#
StringBufferToLowerCasePipe <- R6Class(
    
  "StringBufferToLowerCasePipe",
  
  inherit = PipeGeneric,
    
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StringBufferToLowerCasePipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },        
    
    pipe = function(instance) {
            
      if (!"Instance" %in% class(instance)) {
          stop("[StringBufferToLowerCasePipe][pipe][Error] 
                  Checking the type of the variable: instance ", 
                    class(instance))
      }
      
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "StringBufferToLowerCasePipe")
      
      if (!super$checkCompatibility("StringBufferToLowerCasePipe")) {
        stop("[StringBufferToLowerCasePipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      
      instance$getData() %>>% 
        self$toLowerCase() %>>%
          instance$setData()
      
      return(instance)
    },
        
    toLowerCase = function(data) {

      if (!"character" %in% class(data)) {
          stop("[StringBufferToLowerCasePipe][toLowerCase][Error] 
                  Checking the type of the variable: data ",
                    class(data))
      }
      
      return(data %>>% tolower())
    }
  )
)
