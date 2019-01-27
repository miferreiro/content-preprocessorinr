#Class to obtain the length of the data
#
# 
#
#Variables:
#
MeasureLengthFromStringBufferPipe <- R6Class(
    
  "MeasureLengthFromStringBufferPipe",
  
  inherit = PipeGeneric,
    
  public = list(
        
    initialize = function(propertyName = "length") {
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
        stop("[MeasureLengthFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
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
            stop("[MeasureLengthFromStringBufferPipe][pipe][Error] 
                    Checking the type of the variable: instance ", 
                      class(instance))
        }
        
        if (!"character" %in% class(propertyName)) {
            stop("[MeasureLengthFromStringBufferPipe][pipe][Error] 
                    Checking the type of the variable: propertyName ", 
                      class(propertyName))
        }

        if (!"logical" %in% class(nchar_conf)) {
            stop("[MeasureLengthFromStringBufferPipe][pipe][Error] 
                    Checking the type of the variable: nchar_conf ", 
                      class(nchar_conf))
        }
        
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
      #   instance: (Instance) instance to preprocces
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #            
      if (!"character" %in% class(data)) {
        stop("[MeasureLengthFromStringBufferPipe][getLength][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      if (!"logical" %in% class(nchar_conf)) {
        stop("[MeasureLengthFromStringBufferPipe][getLength][Error] 
                Checking the type of the variable: nchar_conf ", 
                  class(nchar_conf))
      }
        
      return(ifelse(nchar_conf, nchar(data), object.size(data)))
    }
  )
)
