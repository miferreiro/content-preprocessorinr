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
    
    pipe = function(instance, removeEmoji = TRUE) {
        
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
      
      emojisLocated <- list()
      
      
      emojisList <- as.list(rtweet::emojis[2][[1]])
      names(emojisList) <- as.list(rtweet::emojis[[1]][])
      # emojisList <- as.list(emojis_sentiment[[2]])
      # names(emojisList) <- as.list(emojis_sentiment[[1]])
      
      for (emoji in names(emojisList)) {

        if (self$findEmoji(instance$getData(), emoji)) {  
          print(emoji)
          emojisLocated <- list.append(emojisLocated, 
                                              emoji) 
        }
        
        if (removeEmoji && emoji %in% emojisLocated) {
          
          instance$getData() %>>%
            {self$replaceEmoji(emoji, 
                               emojisList[[emoji]],
                                    .)} %>>%
              instance$setData()
        }
      }     
      
      instance$addProperties(paste(emojisLocated),
                             super$getPropertyName()) 
      

      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Emoji")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
      return(instance);
    },
        
    findEmoji = function(data, emoji) {
        
      if (!"character" %in% class(data)) {                    
        stop("[FindEmojiInStringBufferPipe][findEmoji][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(emoji)) {                    
        stop("[FindEmojiInStringBufferPipe][findEmoji][Error] 
                Checking the type of the variable: emoji ", 
                  class(emoji))
      }

      return(grepl(pattern = rex::escape(emoji), x = data, fixed = T, useBytes = T))
      
    },    
    
    replaceEmoji = function(emoji, extendedEmoji, data ) {
      
      if (!"character" %in% class(data)) {
        stop("[FindEmojiInStringBufferPipe][replaceEmoji][Error] 
                Checking the type of the variable: data ", 
             class(data))
      }
      
      if (!"character" %in% class(emoji)) {                    
        stop("[FindEmojiInStringBufferPipe][replaceEmoji][Error] 
                Checking the type of the variable: emoji ", 
             class(emoji))
      }
      
      if (!"character" %in% class(extendedEmoji)) {
        stop("[FindEmojiInStringBufferPipe][replaceEmoji][Error] 
                Checking the type of the variable: extendedEmoji ", 
             class(extendedEmoji))
      }     
      
      return(gsub(rex::escape(emoji), 
                  paste(" ", extendedEmoji, " ", sep = ""), data, perl = T))
    },
    
    replaceEmoji2 = function(data) {
        return(data %>>% replace_emoji())
    }
  )
)
