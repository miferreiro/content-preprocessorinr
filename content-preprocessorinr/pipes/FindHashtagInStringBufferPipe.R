#Class to find and/or replace the hashtags on the data
#
#Variables:
#
#hashtagPattern: (character) Regular expression to detect hashtag
# 
FindHashtagInStringBufferPipe <- R6Class(
        
  "FindHashtagInStringBufferPipe",
        
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "hashtag",  
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
        stop("[FindHashtagInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindHashtagInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindHashtagInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
            
    hashtagPattern = "(?:\\s|^|[\"><¡¿?!;:,.'-])(#[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",          
    
    pipe = function(instance, removeHashtag = TRUE){
      #
      #Function that preprocesses the instance to obtain/replace the hashstags
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #   removeHashtag: (logical) indicate if the hashtags are removed
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #                
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
      
      TypePipe[["private_fields"]][["flowPipes"]] <- 
        list.append(TypePipe[["private_fields"]][["flowPipes"]], "FindHashtagInStringBufferPipe")
      
      if (!super$checkCompatibility("FindHashtagInStringBufferPipe")) {
        stop("[FindHashtagInStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
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
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Hashtag")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindHashtagInStringBufferPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance);
    },
    
    findHashtag = function(data){
      #
      #Function that find the hashtags in the data
      #
      #Args:
      #   data: (character) instance to preproccess
      #Returns:
      #   list with hashtags found
      #           
      if (!"character" %in% class(data)) {
        stop("[FindHashtagInStringBufferPipe][findHashtag][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_match_all(data,
                           regex(self$hashtagPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },
    
    replaceHashtag = function(data){
      #
      #Function that remove the hashtags in the data 
      #
      #Args:
      #   data: (character) instance to preproccess
      #Returns:
      #   data with hashtags removed
      #              
      if (!"character" %in% class(data)) {
        stop("[FindHashtagInStringBufferPipe][replaceHashtag][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(str_replace_all(data,
                             regex(self$hashtagPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)