#Class to 
#
#
#Variables:
#
#
GuessDateFromFilePipe <- R6Class(
    
  "GuessDateFromFilePipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "date") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[GuessDateFromFilePipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
    
    pipe = function(instance) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[GuessDateFromFilePipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      instance$obtainDate()
      
      return(instance)
    }
  )
)
