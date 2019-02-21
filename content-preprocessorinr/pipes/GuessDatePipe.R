#Class to obtain the date
#
#The method of obtaining date is called which implement the subclasses of 
#the superclass Instance
#
#Variables:
#
GuessDatePipe <- R6Class(
    
  "GuessDatePipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "date",  
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
        stop("[GuessDatePipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[GuessDatePipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[GuessDatePipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance) {
      #
      #Function that preprocesses the instance to obtain the date
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #              
      if (!"Instance" %in% class(instance)) {
        stop("[GuessDatePipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      instance$addFlowPipes("GuessDatePipe")
      
      if (!instance$checkCompatibility("GuessDatePipe", self$getAlwaysBeforeDeps())) {
        stop("[GuessDatePipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$obtainDate()
      
      return(instance)
    }
  )
)
