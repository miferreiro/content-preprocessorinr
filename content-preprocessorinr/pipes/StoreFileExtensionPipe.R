#Class to get the extension of a file
#
#
#Variables:
#
StoreFileExtensionPipe <- R6Class(
    
  "StoreFileExtensionPipe",
  
  inherit = PipeGeneric,
    
  public = list(
    
    initialize = function(propertyName = "extension",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      # 
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated, 
      #and the place where the resources of the interjections are stored. 
      #
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
      #
      #Function that preprocesses the instance to obtain the extension of instance
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      # 
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #                 
      if (!"Instance" %in% class(instance)) {
        stop("[StoreFileExtensionPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      TypePipe[["private_fields"]][["flowPipes"]] <- 
        list.append(TypePipe[["private_fields"]][["flowPipes"]], "StoreFileExtensionPipe")
      
      if (!super$checkCompatibility("StoreFileExtensionPipe")) {
        stop("[StoreFileExtensionPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$getPath() %>>% 
        self$obtainExtension() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("extension") %in% "" ) {
        
        message <- c( "The file: " , instance$getPath() , " has not an extension")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[StoreFileExtensionPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
        
      }
            
      return(instance)
    },
        
    obtainExtension = function(path) {
      #
      #Getter of extension of the path
      #
      #Args:
      #   null
      #
      #Returns:
      #   extension of the path
      #      
      if (!"character" %in% class(path)) {
          stop("[StoreFileExtensionPipe][obtainExtension][Error] 
                  Checking the type of the variable: path ", 
                    class(path))
      }
        
      return(file_ext(path))
    }
  )
)
