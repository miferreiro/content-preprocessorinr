#Class to find and/or replace the emoji on the data
#
#Variables:
#
#
FindEmojiPipe <- R6Class(
    
  "FindEmojiPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "Emojis",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {

      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe      #
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
        stop("[FindEmojiPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindEmojiPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindEmojiPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
    }, 
    
    pipe = function(instance, replaceEmoji = TRUE) {
      #
      #Function that preprocesses the instance to obtain/replace the emojis
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   replaceEmoji: (logical) indicate if the emojis are replaced
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #               
      if (!"Instance" %in% class(instance)) {
        stop("[FindEmojiPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }
        
      if (!"logical" %in% class(replaceEmoji)) {
        stop("[FindEmojiPipe][pipe][Error]
                Checking the type of the variable: replaceEmoji ", 
                  class(replaceEmoji))
      }
      
      instance$addFlowPipes("FindEmojiPipe")
      
      if (!instance$checkCompatibility("FindEmojiPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindEmojiPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      emojisLocated <- list()
      
      emojisList <- as.list(rtweet::emojis[2][[1]])
      names(emojisList) <- as.list(rtweet::emojis[[1]][])

      for (emoji in names(emojisList)) {

        if (self$findEmoji(instance$getData(), emoji)) {  
          
          cat("[FindEmojiPipe][pipe][Info] ", paste(emoji), " \n")
          emojisLocated <- list.append(emojisLocated, emoji) 
        }
        
        if (replaceEmoji && emoji %in% emojisLocated) {
          
          instance$getData() %>>%
            {self$replaceEmoji(emoji, emojisList[[emoji]], .)} %>>%
              trim() %>>%
                instance$setData()
        }
      }     
      
      instance$addProperties(paste(emojisLocated),super$getPropertyName()) 

      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Emoji")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[FindEmojiPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      return(instance)
    },
        
    findEmoji = function(data, emoji) {
      #
      #Function that checks if the emoji is in the data
      #
      #Args:
      #   data: (character) instance to preproccess
      #   emoji: (character) indicate if the emoji are found
      #Returns:
      #   TRUE or FALSE depending on whether the emoji is on the data
      #           
      if (!"character" %in% class(data)) {                    
        stop("[FindEmojiPipe][findEmoji][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(emoji)) {                    
        stop("[FindEmojiPipe][findEmoji][Error] 
                Checking the type of the variable: emoji ", 
                  class(emoji))
      }

      return(grepl(pattern = rex::escape(emoji), x = data, fixed = T, useBytes = T))
      
    },    
    
    replaceEmoji = function(emoji, extendedEmoji, data ) {
      #
      #Function that replace the emoji in the data for the extendedEmoji
      #
      #Args:
      #   data: (character) instance to preproccess
      #   emoji: (character) indicate the emoji to remove
      #   extendedEmoji: (character) indicate the string to replace for the emoji
      #Returns:
      #   data with emoji replaced
      #          
      if (!"character" %in% class(data)) {
        stop("[FindEmojiPipe][replaceEmoji][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(emoji)) {                    
        stop("[FindEmojiPipe][replaceEmoji][Error] 
                Checking the type of the variable: emoji ", 
                  class(emoji))
      }
      
      if (!"character" %in% class(extendedEmoji)) {
        stop("[FindEmojiPipe][replaceEmoji][Error] 
                Checking the type of the variable: extendedEmoji ", 
                  class(extendedEmoji))
      }     
      
      return(gsub(rex::escape(emoji), 
                  paste(" ", extendedEmoji, " ", sep = ""), data, perl = T))
    }
  )
)
