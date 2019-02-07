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
    
    initialize = function(propertyName = "Emoticon",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      
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

      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "FindEmoticonInStringBufferPipe")
      
      if (!super$checkCompatibility("FindEmoticonInStringBufferPipe")) {
        stop("[FindEmoticonInStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      
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
      
      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Emoticon")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
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
        
      return(str_match_all(data,
                           regex(self$emoticonPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },           
    
    replaceEmoticon2 = function(data){
        return(data %>>% replace_emoticon())
    }
  )
)