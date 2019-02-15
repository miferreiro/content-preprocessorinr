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

    initialize = function(propertyName = "source",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #            
      if (!"character" %in% class(propertyName)) {
        stop("[File2StringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[File2StringBufferPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[File2StringBufferPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance){
      #
      #Function that preprocesses the instance to obtain the source
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[File2StringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      instance$addFlowPipes("File2StringBufferPipe")
      
      if (!instance$checkCompatibility("File2StringBufferPipe", self$getAlwaysBeforeDeps())) {
        stop("[File2StringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$obtainSource()
        
      if (is.na(instance$getSource()) || all(instance$getSource() == "") || is.null(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " has source empty")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[File2StringBufferPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      if (!validUTF8(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " is not utf8")
        
        instance$addProperties(message, "reasonToInvalidate")  
        
        cat("[File2StringBufferPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
        
      return(instance)
    }
  )
)
