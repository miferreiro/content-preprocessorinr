#Class to 
#
#
#Variables:
#
#
FindHashtagInStringBufferPipe <- R6Class(
        
  "FindHashtagInStringBufferPipe",
        
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "hashtag") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[FindHashtagInStringBufferPipe][initialize][Error] 
             Checking the type of the variable: propertyName ", class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },
            
    hashtagPattern = "(?:\\s|^|[\"><¡?¿!;:,.'-])(#[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",          
    
    pipe = function(instance, removeHashtag = TRUE){
              
      if (!"Instance" %in% class(instance)) {
        stop("[FindHashtagInStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
          
      if (!"logical" %in% class(removeHashtag)) {
        stop("[FindHashtagInStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeHashtag ", 
                  class(removeHashtag))
      }
      
      instance$getData() %>>% 
        self$findHashtag() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}
      
      if (removeHashtag) {
          instance$getData()  %>>%
            self$replaceHashtag() %>>%
              {instance$setData(.)}
      }    
      
      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Hashtag")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
      
      return(instance);
    },
          
    replaceHashtag = function(data){
        
      if (!"character" %in% class(data)) {
        stop("[FindHashtagInStringBufferPipe][replaceHashtag][Error] 
             Checking the type of the variable: data ", class(data))
      }
        
      return(str_replace_all(data,
                             regex(self$hashtagPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    },
    
    findHashtag = function(data){
        
      if (!"character" %in% class(data)) {
        stop("[FindHashtagInStringBufferPipe][findHashtag][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_match_all(data,
                          regex(self$hashtagPattern,
                                ignore_case = TRUE,
                                multiline = TRUE))[[1]][,2])
    }
  )
)