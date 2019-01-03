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
      private$propertyName <- propertyName
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
