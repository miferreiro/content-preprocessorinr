#Class to find and/or replace the emoticon on the data
#
#Variables:
#emoticonPattern: (character) Regular expression to detect emoticons
#
FindEmoticonInStringBufferPipe <- R6Class(
    
  "FindEmoticonInStringBufferPipe",
    
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "Emoticon",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list("FindHashtagInStringBufferPipe")) {
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
        stop("[FindEmoticonInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindEmoticonInStringBufferPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindEmoticonInStringBufferPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
    }, 

    emoticonPattern = '(\\:\\w+\\:|\\<[\\/\\\\]?3|[\\(\\)\\\\\\D|\\*\\$][\\-\\^]?[\\:\\;\\=]|[\\:\\;\\=B8][\\-\\^]?[3DOPp\\@\\$\\*\\\\\\)\\(\\/\\|])(?=\\s|[\\!\\.\\?\\:\\w<>]|$)',
        
    pipe = function(instance, removeEmoticon = TRUE){
      #
      #Function that preprocesses the instance to obtain/replace the emoticon
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   removeEmoticon: (logical) indicate if the emoticon are removed
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #               
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

      instance$addFlowPipes("FindEmoticonInStringBufferPipe")
      
      if (!instance$checkCompatibility("FindEmojiInStringBufferPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindEmoticonInStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getData() %>>%
        self$findEmoticon() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}
      
      if (removeEmoticon) {
          instance$getData()  %>>%
            self$replaceEmoticon() %>>%
              instance$setData()
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " hsas data empty on pipe Emoticon")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindEmoticonInStringBufferPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findEmoticon = function(data){
      #
      #Function that find the emoticons in the data
      #
      #Args:
      #   data: (character) instance to preproccess
      #Returns:
      #   list with emoticons found
      #       
      if (!"character" %in% class(data)) {                    
        stop("[FindEmoticonInStringBufferPipe][findEmoticon][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_match_all(data,
                           regex(self$emoticonPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },
        
    replaceEmoticon = function(data){
      #
      #Function that remove the emoticons in the data 
      #
      #Args:
      #   data: (character) instance to preproccess
      #Returns:
      #   data with emoticons removed
      #            
      if (!"character" %in% class(data)) {
        stop("[FindEmoticonInStringBufferPipe][replaceEmoticon][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(str_replace_all(data,
                             regex(self$emoticonPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)