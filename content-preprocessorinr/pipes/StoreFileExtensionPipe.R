StoreFileExtensionPipe <- R6Class(
    
  "StoreFileExtensionPipe",
    
  public = list(
    
    pipe = function(instance) {
            
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[StoreFileExtensionPipe][pipe][Error] 
               Checking the type of the variable: instance ", class(instance))
      }
      
      instance$getPath() %>>% 
        self$getExtension() %>>%
            {instance$addProperties(.,self$getPropertyName())}
      
      return(instance)
    },
        
    getExtension = function(path) {
      
      if (!"character" %in% class(path)) {
          stop("[StoreFileExtensionPipe][getExtension][Error] 
               Checking the type of the variable: path ", class(path))
      }
        
      return(file_ext(path))
    },
    
    getPropertyName = function() {
      return(private$propertyName)
    }
  ),
  
  private = list(
    propertyName = "extension"
  )
)
