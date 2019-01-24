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
    
    initialize = function(propertyName = "extension") {

      if (!"character" %in% class(propertyName)) {
        stop("[StoreFileExtensionPipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
    
    pipe = function(instance) {
            
      if (!"Instance" %in% class(instance)) {
        stop("[StoreFileExtensionPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      instance$getPath() %>>% 
        self$getExtension() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("extension") %in% "" ) {
        message <- c( "The file: " , instance$getPath() , " has not an extension")
        warning(message)         
        
        instance$invalidate()
        return(NULL)
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
