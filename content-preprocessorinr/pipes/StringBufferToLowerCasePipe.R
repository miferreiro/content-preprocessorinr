#Class to 
#
#
#Variables:
#
#
StringBufferToLowerCasePipe <- R6Class(
    
  "StringBufferToLowerCasePipe",
  
  inherit = PipeGeneric,
    
  public = list(
    
    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StringBufferToLowerCasePipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },        
    
    pipe = function(instance) {
            
      if (!"ExtractorSource" %in% class(instance)) {
          stop("[StringBufferToLowerCasePipe][pipe][Error] 
                  Checking the type of the variable: instance ", 
                    class(instance))
      }
      
      instance$getData() %>>% 
        self$toLowerCase() %>>%
          instance$setData()
      
      return(instance)
    },
        
    toLowerCase = function(data) {

      if (!"character" %in% class(data)) {
          stop("[StringBufferToLowerCasePipe][toLowerCase][Error] 
                  Checking the type of the variable: data ",
                    class(data))
      }
      
      return(data %>>% tolower())
    }
  )
)
