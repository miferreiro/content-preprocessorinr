#Class to 
#
#
#Variables:
#
#
StoreFileExtensionPipe <- R6Class(
    
  "StoreFileExtensionPipe",
  
  inherit = PipeGeneric,
    
  public = list(
    
    initialize = function(propertyName = "extension",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[StoreFileExtensionPipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StoreFileExtensionPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StoreFileExtensionPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance) {
            
      if (!"Instance" %in% class(instance)) {
        stop("[StoreFileExtensionPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "StoreFileExtensionPipe")
      
      if (!super$checkCompatibility("StoreFileExtensionPipe")) {
        stop("[StoreFileExtensionPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      
      instance$getPath() %>>% 
        self$getExtension() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("extension") %in% "" ) {
        message <- c( "The file: " , instance$getPath() , " has not an extension")
        instance$addProperties(message, "reasonToInvalidate") 
        warning(message)         
        
        instance$invalidate()
        return(instance)
      } else {
        return(instance)
      }
            
      return(instance)
    },
        
    getExtension = function(path) {
      
      if (!"character" %in% class(path)) {
          stop("[StoreFileExtensionPipe][getExtension][Error] 
                  Checking the type of the variable: path ", 
                    class(path))
      }
        
      return(file_ext(path))
    }
  )
)
