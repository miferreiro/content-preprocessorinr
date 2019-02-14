#Super class that handles the general functionalities of the management of
#the pipes
#
#Variables:
#
#propertyName: (character) the name of property 
#alwaysBeforeDeps: (list) Dependencies of the type alwaysBefore. These dependences 
#                         indicate what pipes must be executed before the current one.
#notAfterDeps: (list) Dependencies of the type notAfter. These dependences 
#                     indicate what pipes must not be executed after the current one.
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
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
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
      #
      #Getter of the dependences always before
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of dependences always before
      #
      return(private$alwaysBeforeDeps)
    },
    
    getNotAfterDeps = function() {
      #
      #Getter of the dependences not after
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of dependences not after
      #
      return(private$notAfterDeps)
    },
    
    checkCompatibility = function(namePipe) {
      #
      #Check compability between pipes.
      #
      #Args:
      #   namePipe: (character) name of the pipe to check the compatibility
      #
      #Returns:
      #   TRUE/FALSE depends if the compability between pipes is correctly or not
      #      
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
    alwaysBeforeDeps = list() ,
    notAfterDeps = list()
  )
)
