#Super class that handles the general functionalities of the management of
#the pipes
#
#Variables:
#
#propertyName: (character) the name of property 
#
PipeGeneric <- R6Class(
  
  "PipeGeneric",
  
  public = list(
        
    initialize = function(propertyName, alwaysBeforeDeps, notAfterDeps) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable
      #contains the property's name of the pipe
      #In addition, the initial path property is initialized, just in case
      #the path of the file is changed and it is decided to save
      #the file in cache.
      #
      #Args:
      #   propertyName: (character) Property's name of the pipe
      #
      #Returns:
      #   null
      #      
      if (!"character" %in% class(propertyName)) {
        stop("[PipeGeneric][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[PipeGeneric][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[PipeGeneric][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      private$propertyName <- propertyName
      private$alwaysBeforeDeps <- alwaysBeforeDeps
      private$notAfterDeps <- notAfterDeps
       # cat("Initialize of ", self$getPropertyName(),"\n")
    },    
    
    pipe = function(instance) {
      #
      #Abtract method to process the intance
      #
      #Args:
      #   instance: (Instance)
      #
      #Returns:
      #   null
      #
      stop("I'm an abstract interface method")
    },
    

    getPropertyName = function() {
      #
      #Getter of name of property
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of propertyName variable
      #
      return(private$propertyName)
    },
    
    getAlwaysBeforeDeps = function() {
      return(private$alwaysBeforeDeps)
    },
    getNotAfterDeps = function() {
      return(private$notAfterDeps)
    },
    
    checkCompatibility = function(namePipe) {
      
      flowPipes <- TypePipe[["private_fields"]][["flowPipes"]]
      banPipes <- TypePipe[["private_fields"]][["banPipes"]]
      
      for (depsB in self$getAlwaysBeforeDeps()) {
        
        if (!depsB %in% flowPipes) {
          return(FALSE)
        }
      }
      
      if (namePipe %in% banPipes) {
        return(FALSE)
      }
      
      return(TRUE)
    }
    
  ),
  
  private = list(
    propertyName = "",
    # Dependencies of the type alwaysBefore
    # These dependences indicate what pipes must be
    # executed before the current one.
    alwaysBeforeDeps = list() ,
    # Dependencies of the type notAfter
    # These dependences indicate what pipes must not be
    # executed after the current one.
    notAfterDeps = list()
  )
)
