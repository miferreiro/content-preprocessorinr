#Class to find and/or replace the urls on the data
#
#Variables:
# 
#URLPattern: (character) Regular expression to detect urls
#EmailPattern: (character) Regular expression to detect emails
# 
FindUrlInStringBufferPipe <- R6Class(
    
  "FindUrlInStringBufferPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "URLs",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list("FindUserNameInStringBufferPipe")) {
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
        stop("[FindUrlInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindUrlInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindUrlInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },  

    URLPattern = "(?:\\s|[\"><¡¿?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))",

    EmailPattern = "(?:\\s|[\"><¡¿?!;:,.'\\(]|^)((?:[\\w_.çñ+-]+)(?:@|\\(at\\)|<at>)(?:(?:\\w[\\\\.:ñ-]?)*)[[:alnum:]ñ](?:\\.[a-zA-Z]{2,4}))[;:?\"!,.'>\\)]?(?=(?:\\s|$|>|\\.|,))",
    
    pipe = function(instance, 
                    removeUrl = TRUE,
                    URLPatterns = list(self$URLPattern, self$EmailPattern), 
                    namesURLPatterns = list("UrlPattern","EmailPattern")) {
      #
      #Function that preprocesses the instance to obtain/replace the urls
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   removeUrl: (logical) indicate if the urls are removed
      #   URLPatterns: (list) the regex to find urls
      #   namesURLPatterns: (list) the name of regex
      # 
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #         
      if (!"Instance" %in% class(instance)) {
        stop("[FindUrlInStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(removeUrl)) {
        stop("[FindUrlInStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeUrl ", 
                  class(removeUrl))
      }

      if (!"list" %in% class(URLPatterns)) {
        stop("[FindUrlInStringBufferPipe][pipe][Error]
                Checking the type of the variable: URLPatterns ", 
                  class(URLPatterns))
      }

      if (!"list" %in% class(namesURLPatterns)) {
        stop("[FindUrlInStringBufferPipe][pipe][Error]
                 Checking the type of the variable: namesURLPatterns ", 
                   class(namesURLPatterns))
      }                
                          
      instance$addFlowPipes("FindUrlInStringBufferPipe")
      
      if (!instance$checkCompatibility("FindEmojiInStringBufferPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindUrlInStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      private$URLPatterns <- URLPatterns
      private$namesURLPatterns <- namesURLPatterns
      
      instance$getData() %>>%
        {lapply(private$URLPatterns, self$findUrl,.)} %>>%
          self$putNamesURLPattern() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}

      if (removeUrl) {
        for (pattern in self$getURLPatterns()) {
          instance$getData() %>>%
            {self$replaceUrl(pattern,.)} %>>%
              instance$setData()
        }
      }
        
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Url")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindUrlInStringBufferPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findUrl = function(pattern, data) {
      #
      #Function that find the urls in the data
      #
      #Args:
      #   pattern: (character) regex to find urls
      #   data: (character) text to find urls
      #Returns:
      #   list with urls found
      #               
      if (!"character" %in% class(pattern)) {
        stop("[FindUrlInStringBufferPipe][findUrl][Error] 
             Checking the type of the variable: pattern ", 
             class(pattern))
      }               
      
      if (!"character" %in% class(data)) {
        stop("[FindUrlInStringBufferPipe][findUrl][Error] 
             Checking the type of the variable: data ", 
             class(data))
      }
      
      return(str_match_all(data,
                           regex(pattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2] %>>% unique() %>>% unlist() )
    },  
    
    replaceUrl = function(pattern, data) {
      #
      #Function that remove the urls in the data 
      #
      #Args:
      #   pattern: (character) regex to find urls
      #   data: (character) instance to preproccess
      #Returns:
      #   data with urls removed
      #        
      
      if (!"character" %in% class(pattern)) {
        stop("[FindUrlInStringBufferPipe][replaceUrl][Error] 
                Checking the type of the variable: pattern ", 
                  class(pattern))
      }               
        
      if (!"character" %in% class(data)) {
        stop("[FindUrlInStringBufferPipe][replaceUrl][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(str_replace_all(data,
                              regex(pattern,
                                    ignore_case = TRUE,
                                    multiline = TRUE), " "))

    },
      
    putNamesURLPattern = function(resultOfURLPatterns) {
      #
      #Set the names to url patterns result
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of resultOfURLPatterns variable with the names of url pattern
      #        
      if (!"list" %in% class(resultOfURLPatterns)) {
        stop("[FindUrlInStringBufferPipe][putNamesPattern][Error] 
                Checking the type of the variable: resultOfURLPatterns ", 
                  class(resultOfURLPatterns))
      }
        
      names(resultOfURLPatterns) <- self$getNamesURLPatterns() 
      
      return(resultOfURLPatterns)          
    },
      
    getURLPatterns = function() {
      #
      #Getter of url patterns
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of URLPatterns variable
      #
      return(private$URLPatterns)
    },
      
    getNamesURLPatterns = function() {
      #
      #Getter of name of urls
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of namesURLPatterns variable
      #
      return(private$namesURLPatterns)
    }
  ),  
  
  private = list(
    URLPatterns = list(),
    namesURLPatterns = list()
  )
)
