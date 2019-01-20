#Class to 
#
#
#Variables:
#
#
FindEmoticonInStringBufferPipe <- R6Class(
    
  "FindEmoticonInStringBufferPipe",
    
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "Emoticon") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[FindEmoticonInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 

    emoticonPattern = '(\\:\\w+\\:|\\<[\\/\\\\]?3|[\\(\\)\\\\\\D|\\*\\$][\\-\\^]?[\\:\\;\\=]|[\\:\\;\\=B8][\\-\\^]?[3DOPp\\@\\$\\*\\\\\\)\\(\\/\\|])(?=\\s|[\\!\\.\\?]|$)',
        
    pipe = function(instance, removeEmoticon = TRUE){
            
      if (!"Instance" %in% class(instance)) {
        stop("[FindEmoticonInStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(removeEmoticon)) {
        stop("[FindEmoticonInStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeEmoticon ", 
                  class(removeEmoticon))
      }

      instance$getData() %>>%
        self$findEmoticon() %>>%
          unlist() %>>%
            {instance$addProperties(.,super$getPropertyName())}
      
      if (removeEmoticon) {
          instance$getData()  %>>%
            self$replaceEmoticon() %>>%
              instance$setData()
      }
      
      return(instance)
    },
        
    replaceEmoticon = function(data){
        
      if (!"character" %in% class(data)) {
        stop("[FindEmoticonInStringBufferPipe][replaceEmoticon][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(str_replace_all(data,
                             regex(self$emoticonPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    },
    
    findEmoticon = function(data){
        
      if (!"character" %in% class(data)) {                    
          stop("[FindEmoticonInStringBufferPipe][findEmoticon][Error] 
                  Checking the type of the variable: data ", 
                    class(data))
      }
        
      return(str_extract_all(data,
                             regex(self$emoticonPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE)))
    },           
    
    replaceEmoticon2 = function(data){
        return(data %>>% replace_emoticon())
    }
  )
)