FindEmoticonInStringBufferPipe <- R6Class(
    
  "FindEmoticonInStringBufferPipe",
    
  public = list(

    emoticonPattern = '(\\:\\w+\\:|\\<[\\/\\\\]?3|[\\(\\)\\\\\\D|\\*\\$][\\-\\^]?[\\:\\;\\=]|[\\:\\;\\=B8][\\-\\^]?[3DOPp\\@\\$\\*\\\\\\)\\(\\/\\|])(?=\\s|[\\!\\.\\?]|$)',
        
    pipe = function(instance, removeEmoticon = TRUE){
            
      if (!"ExtractorSource" %in% class(instance)) {
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
              {instance$addProperties(.,self$getPropertyName())}
      
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
               Checking the type of the variable: data ", class(data))
      }
        
      return(str_replace_all(data,
                             regex(self$emoticonPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    },
    
    findEmoticon = function(data){
        
      if (!"character" %in% class(data)) {                    
          stop("[FindEmoticonInStringBufferPipe][findEmoticon][Error] 
               Checking the type of the variable: data ", class(data))
      }
        
      return(str_extract_all(data,
                             regex(self$emoticonPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE)))
    },           
    
    replaceEmoticon2 = function(data){
        return(data %>>% replace_emoticon())
    },
    
    getPropertyName = function(){
        return(private$propertyName)
    }
  ),  
  private = list(
    propertyName = "Emoticon"
  )
)