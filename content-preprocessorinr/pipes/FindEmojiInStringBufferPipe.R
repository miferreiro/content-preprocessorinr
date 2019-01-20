#Class to 
#
#
#Variables:
#
#
FindEmojiInStringBufferPipe <- R6Class(
    
  "FindEmojiInStringBufferPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "Emojis") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[FindEmojiInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
    
    emojiPattern = "",
        
    pipe = function(instance, removeEmoji = FALSE) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[FindEmojiInStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }
        
      if (!"logical" %in% class(removeEmoji)) {
        stop("[FindEmojiInStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeEmoji ", 
                  class(removeEmoji))
      }
      
      # instance$getData() %>>%
      #     self$findEmoji() %>>%
      #         {instance$addProperties(.,super$getPropertyName())}
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
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_replace_all(data,
                             regex(self$emojiPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    },
    
    findEmoji = function(data) {
        
      if (!"character" %in% class(data)) {                    
        stop("[FindEmojiInStringBufferPipe][findEmoji][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_extract_all(data,
                             regex(self$emojiPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE)))
    },    
    
    replaceEmoji2 = function(data) {
        return(data %>>% replace_emoji())
    }
  )
)
