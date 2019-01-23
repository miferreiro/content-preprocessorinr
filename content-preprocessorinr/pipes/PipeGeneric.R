#Class to 
#
#
#Variables:
#propertyName: (character) 
#targets: (list) 
#
PipeGeneric <- R6Class(
  
  "PipeGeneric",
  
  public = list(
        
    initialize = function(propertyName) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[PipeGeneric][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      private$propertyName <- propertyName
      
       # cat("Initialize of ", self$getPropertyName(),"\n")
    },    
    
    pipe = function(instance) {
      stop("I'm an abstract interface method")
    },
    

    getPropertyName = function() {
      return(private$propertyName)
    }
    
  ),
  
  private = list(
    propertyName = ""
  )
)
