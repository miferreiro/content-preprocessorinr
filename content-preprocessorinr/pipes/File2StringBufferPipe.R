#Class to obtain the source
#
# The method of obtaining source is called which implement the subclasses of 
# the superclass Instance
#
#Variables:
#
File2StringBufferPipe <- R6Class(
    
  "File2StringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "source") {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #
      #Args:
      #   propertyName: (character) Name of the property
      #
      #Returns:
      #   null
      #            
      if (!"character" %in% class(propertyName)) {
        stop("[File2StringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
    
    pipe = function(instance){
      #
      #Function that preprocesses the instance to obtain the source
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #           
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
      
      if ( all(instance$getSource() == "") || is.null(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " has source empty")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
        
      return(instance);
    }
  )
)
