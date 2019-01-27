#Class to obtain the date
#
# The method of obtaining date is called which implement the subclasses of 
# the superclass Instance
#
#Variables:
#
GuessDateFromFilePipe <- R6Class(
    
  "GuessDateFromFilePipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "date") {
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
        stop("[GuessDateFromFilePipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
    
    pipe = function(instance) {
      #
      #Function that preprocesses the instance to obtain the date
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #              
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
