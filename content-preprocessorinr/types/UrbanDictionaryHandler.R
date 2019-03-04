UrbanDictionaryHandler <- R6Class(
  
  "UrbanDictionaryHandler",
  
  inherit = UnmatchedTextHandler,
  
  public = list(
    
    initialize = function(pathResourcesSlangs = "content-preprocessorinr/resources/slangs-json") {
      
      if (!"character" %in% class(pathResourcesSlangs)) {
        stop("[UrbanDictionaryHandler][initialize][Error] 
                Checking the type of the variable: pathResourcesSlangs ", 
                  class(pathResourcesSlangs))
      }
      
      private$pathResourcesSlangs <- pathResourcesSlangs
    },
    
    handle = function(originalText, replacementText, lang) {
      
      if (!"character" %in% class(originalText)) {
        stop("[UrbanDictionaryHandler][handle][Error] 
                Checking the type of the variable: originalText ", 
                  class(originalText))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][initialize][Error] 
                Checking the type of the variable: lang ", 
             class(lang))
      }
      
      matchedString <- NULL
      
      if (is.null(replacementText)) {
        matchedString <- self$getReplacement4SlangTerm(originalText, lang)
        if (!is.null(matchedString)) {
          cat("[UrbanDictionaryHandler][handle][Info]"," Sucessfull match for string ", matchedString, "\n")
        }
      }
      
      return(matchedString)
    },
    
    getReplacement4SlangTerm = function(slangTerm, lang) {
      
      JsonFile <- paste(self$getPathResourcesSlangs(),
                        "/slang.",
                        tolower(lang),
                        ".json",
                        sep = "") 
      
      jsonData <- resourceHandle$isLoadResource(JsonFile)
      
      if (is.null(jsonData)) { 
        message <- c( "Has not an SlangsJsonFile to apply to the language -> ", tolower(lang))
          
        cat("[UrbanDictionaryHandler][getReplacement4SlangTerm][Warning] ", message, " \n")
        return(NULL)
      }
      
      if (!slangTerm %in% names(jsonData)) {
        return(NULL)
      }
      return(jsonData)[[slangTerm]]
    },
      
    getPathResourcesSlangs = function() {
      #
      #Getter of path of slangs resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pathResourcesSlangs variable
      #
      return(private$pathResourcesSlangs)
    }
  ),
  
  private = list(
    pathResourcesSlangs = ""
  )
)