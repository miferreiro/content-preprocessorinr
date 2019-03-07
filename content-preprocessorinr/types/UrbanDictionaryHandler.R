#A class to get the match with the specific word from an Urban dictionary
#
#Variables:
#
#pathResourcesSlangs: (character) the path where are the resources
# 
UrbanDictionaryHandler <- R6Class(
  
  "UrbanDictionaryHandler",
  
  inherit = UnmatchedTextHandler,
  
  public = list(
    
    initialize = function(pathResourcesSlangs = "content-preprocessorinr/resources/slangs-json") {
      #
      #Class constructor
      #
      #This constructor initialize the variable which contains the place where 
      #the resources of the abbreviations are stored. 
      #
      #Args:
      #   pathResourcesSlangs: (character) Path where are stored the 
      #                                           slangs resources
      #Returns:
      #   null
      #      
            
      if (!"character" %in% class(pathResourcesSlangs)) {
        stop("[UrbanDictionaryHandler][initialize][Error] 
                Checking the type of the variable: pathResourcesSlangs ", 
                  class(pathResourcesSlangs))
      }
      
      private$pathResourcesSlangs <- pathResourcesSlangs
    },
    
    handle = function(originalText, replacementText, lang) {
      #
      #Get the matches with the originalText and set it to replacementText.
      #
      #Args:
      #   originalText: (Instance)  The original text to replace
      #   replacementText: (logical) The word that matches with the originalText
      #   lang: (character) The language of the original string
      #Returns:
      #   The word that matches with the originalText
      #          
      if (!"character" %in% class(originalText)) {
        stop("[UrbanDictionaryHandler][handle][Error] 
                Checking the type of the variable: originalText ", 
                  class(originalText))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][handle][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      matchedString <- replacementText
      
      if (is.null(replacementText)) {
        matchedString <- self$getReplacement4SlangTerm(originalText, lang)
        if (!is.null(matchedString)) {
          cat("[UrbanDictionaryHandler][handle][Info]"," Sucessfull match for string ", matchedString, "\n")
        }
      }
      
      return(matchedString)
    },
    
    getReplacement4SlangTerm = function(slangTerm, lang) {
      #
      #Get the matches with the originalText and set it to replacementText.
      #
      #Args:
      #   slangTerm: (logical) The original text to replace
      #   lang: (character) The language of the original string
      #Returns:
      #   The word that matches with the originalText
      #           
      if (!"character" %in% class(slangTerm)) {
        stop("[UrbanDictionaryHandler][getReplacement4SlangTerm][Error] 
                Checking the type of the variable: slangTerm ", 
                  class(slangTerm))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][getReplacement4SlangTerm][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
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