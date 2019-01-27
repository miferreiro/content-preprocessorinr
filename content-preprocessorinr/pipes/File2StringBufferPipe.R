#Class to 
#
#
#Variables:
#
#
File2StringBufferPipe <- R6Class(
    
  "File2StringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "source") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[File2StringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
    
    pipe = function(instance){
        
      if (!"Instance" %in% class(instance)) {
        stop("[File2StringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      instance$obtainSource()
        
      
      # if ( !validUTF8(instance$getSource())) {
      #   message <- c( "The file: " , instance$getPath() , " isnt utf8")
      #   warning(message)
      #   
      #   instance$invalidate()
      #   return(instance)
      # }
      
      if ( instance$getSource() == "" || is.null(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " has source empty")
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
        
      return(instance);
    }
  )
)
