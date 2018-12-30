
FindEmojiInStringBufferPipe <- R6Class(
    
  "FindEmojiInStringBufferPipe",
    
  public = list(
        
    emojiPattern = "",
        
    pipe = function(instance, removeEmoji = FALSE) {
        
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[FindEmojiInStringBufferPipe][pipe][Error]
               Checking the type of the variable: instance ", class(instance))
      }
        
      if (!"logical" %in% class(removeEmoji)) {
        stop("[FindEmojiInStringBufferPipe][pipe][Error]
               Checking the type of the variable: removeEmoji ", 
                 class(removeEmoji))
      }
      
      # instance$getData() %>>%
      #     self$findEmoji() %>>%
      #         {instance$addProperties(.,self$getPropertyName())}
      # 
      # if (removeEmoji){
      #     instance$getData()  %>>%
      #         self$replaceEmoji() %>>%
      #             instance$setData()
      # }

      return(instance);
    },
        
    replaceEmoji = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[FindEmojiInStringBufferPipe][replaceEmoji][Error] 
             Checking the type of the variable: data ", class(data))
      }
      
      return(str_replace_all(data,
                             regex(self$emojiPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    },
    
    findEmoji = function(data) {
        
      if (!"character" %in% class(data)) {                    
        stop("[FindEmojiInStringBufferPipe][findEmoji][Error] 
               Checking the type of the variable: data ", class(data))
      }
      
      return(str_extract_all(data,
                             regex(self$emojiPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE)))
    },    
    
    replaceEmoji2 = function(data) {
        return(data %>>% replace_emoji())
    },
    
    getPropertyName = function() {
        return(private$propertyName)
    }
  ),  
  
  private = list(
    propertyName = "Emojis"
  )
)
