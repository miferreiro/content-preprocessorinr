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
        
    initialize = function(propertyName) {
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
      
      private$propertyName <- propertyName
      
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
    }
    
  ),
  
  private = list(
    propertyName = ""
  )
)
