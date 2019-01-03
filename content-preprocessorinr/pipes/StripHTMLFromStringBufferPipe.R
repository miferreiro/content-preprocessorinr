#Class to 
#
#
#Variables:
#
#
StripHTMLFromStringBufferPipe <- R6Class(
    
  "StripHTMLFromStringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StripHTMLFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },  
    
    pipe = function(instance) {
        
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[StripHTMLFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
  
      instance$getData() %>>% 
        self$getDataWithOutHtml() %>>%
          instance$setData()
        
      return(instance);
    },
    
    getDataWithOutHtml = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[StripHTMLFromStringBufferPipe][getDataWithOutHtml][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return( data %>>% replace_html() )
    }
  )  
)
