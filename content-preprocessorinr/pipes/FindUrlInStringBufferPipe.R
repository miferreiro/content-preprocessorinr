#Class to 
#
#
#Variables:
#
#
FindUrlInStringBufferPipe <- R6Class(
    
  "FindUrlInStringBufferPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "URLs") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[FindUrlInStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },  

    URLPattern = "(?:\\s|[\"><¡¿?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))",

    EmailPattern = "(?:\\s|[\"><¡¿?!;:,.'\\(]|^)((?:[\\w_.çñ+-]+)(?:@|\\(at\\)|<at>)(?:(?:\\w[\\\\.:ç-]?)*)[[:alnum:]ñ](?:\\.[A-Z]{2,4}))[;:?\"!,.'>\\)]?(?=(?:\\s|$|>|\\.|,))",
    
    pipe = function(instance, removeUrl = TRUE,
                      URLPatterns = list(self$URLPattern, self$EmailPattern), 
                        namesURLPatterns = list("UrlPattern","EmailPattern")) {
  
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
                                        
      private$URLPatterns <- URLPatterns
      private$namesURLPatterns <- namesURLPatterns
      
      instance$getData() %>>%
        {lapply(private$URLPatterns, self$findUrl,.)} %>>%
          self$putNamesURLPattern() %>>%
            {instance$addProperties(.,super$getPropertyName())}

      if (removeUrl) {
        for (pattern in self$getURLPatterns()) {
          instance$getData() %>>%
            {self$replaceUrl(pattern,.)} %>>%
              instance$setData()
        }
      }
        
      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Url")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
      
      return(instance)
    },
      
    replaceUrl = function(pattern,data) {
        
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
      
    findUrl = function(pattern,data) {
        
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
    
      
    putNamesURLPattern = function(resultOfURLPatterns) {
        
      if (!"list" %in% class(resultOfURLPatterns)) {
        stop("[FindUrlInStringBufferPipe][putNamesPattern][Error] 
                Checking the type of the variable: resultOfURLPatterns ", 
                  class(resultOfURLPatterns))
      }
        
      names(resultOfURLPatterns) <- self$getNamesURLPatterns() 
      
      return(resultOfURLPatterns)          
    },
      
    getURLPatterns = function() {
      return(private$URLPatterns)
    },
      
    getNamesURLPatterns = function() {
      return(private$namesURLPatterns)
    }
  ),  
  
  private = list(
    URLPatterns = list(),
    namesURLPatterns = list()
  )
)
