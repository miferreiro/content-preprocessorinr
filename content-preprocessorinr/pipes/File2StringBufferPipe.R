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
        
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[File2StringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      instance$obtainSource()
        
      ifelse((!validUTF8(instance$getSource())), {
        message <- c( "The file: " , instance$getPath() , " isnt utf8")
        warning(message)
      },"")
        
      return(instance);
    }
  )
)
