#Class to obtain the length of the data
#
#Variables:
#
MeasureLengthPipe <- R6Class(
    
  "MeasureLengthPipe",
  
  inherit = PipeGeneric,
    
  public = list(
        
    initialize = function(propertyName = "length",  
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
        stop("[MeasureLengthPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[MeasureLengthPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[MeasureLengthPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance,
                    propertyName = super$getPropertyName(),
                    nchar_conf = TRUE) {
      #
      #Function that preprocesses the instance to obtain the length of data
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #          
      
        if (!"Instance" %in% class(instance)) {
            stop("[MeasureLengthPipe][pipe][Error] 
                    Checking the type of the variable: instance ", 
                      class(instance))
        }
        
        if (!"character" %in% class(propertyName)) {
            stop("[MeasureLengthPipe][pipe][Error] 
                    Checking the type of the variable: propertyName ", 
                      class(propertyName))
        }

        if (!"logical" %in% class(nchar_conf)) {
            stop("[MeasureLengthPipe][pipe][Error] 
                    Checking the type of the variable: nchar_conf ", 
                      class(nchar_conf))
        }
        
        instance$addFlowPipes("MeasureLengthPipe")
        
        if (!instance$checkCompatibility("MeasureLengthPipe", self$getAlwaysBeforeDeps())) {
          stop("[MeasureLengthPipe][pipe][Error] Bad compatibility between Pipes.")
        }
        
        instance$addBanPipes(unlist(super$getNotAfterDeps()))
        
        instance$getData() %>>% 
          {self$getLength(.,nchar_conf)} %>>%
            {instance$addProperties(.,propertyName)}
        
        return(instance);
    },
    
    getLength = function(data, nchar_conf = TRUE) {
      #
      #Function that obtain the length of data
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   nchar_conf: (logical)
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #            
      if (!"character" %in% class(data)) {
        stop("[MeasureLengthPipe][getLength][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      if (!"logical" %in% class(nchar_conf)) {
        stop("[MeasureLengthPipe][getLength][Error] 
                Checking the type of the variable: nchar_conf ", 
                  class(nchar_conf))
      }
        
      return(ifelse(nchar_conf, nchar(data), object.size(data)))
    }
  )
)
