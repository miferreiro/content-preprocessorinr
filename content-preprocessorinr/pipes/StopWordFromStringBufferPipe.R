#Class to 
#
#
#Variables:
#
#
StopWordFromStringBufferPipe <- R6Class(
  
  "StopWordFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "stopWord",
                          propertyLanguageName = "language",
                          pathResourcesStopWords = "content-preprocessorinr/resources/stopwords-json") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StopWordFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[StopWordFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesStopWords)) {
        stop("[StopWordFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesStopWords ", 
                  class(pathResourcesStopWords))
      }
      
      propertyName %>>% 
        super$initialize()
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesStopWords <- pathResourcesStopWords
    }, 
        
    pipe = function(instance, removeStopWords = TRUE) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[StopWordFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }

      if (!"logical" %in% class(removeStopWords)) {
        stop("[StopWordFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeStopWords ", 
                  class(removeStopWords))
      }    
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      if (is.null(languageInstance) || 
          is.na(languageInstance) || 
          "Unknown" %in% languageInstance) {
        instance$addProperties(list(),
                                super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not language property")
        warning(message)  
        instance$invalidate()
        return(NULL)
      }      


      if (file.exists(paste(self$getPathResourcesStopWords(),
                              "/",
                                languageInstance,
                                  ".json",
                                    sep = ""))) {
        
        JsonFile <- paste(self$getPathResourcesStopWords(),
                            "/",
                              languageInstance,
                                ".json",
                                  sep = "")        
        stopWordLocated <- list()
      
        jsonData <- fromJSON(file = JsonFile)
        
        for (stopWord in jsonData) {
          
          if (self$findStopWord(instance$getData(), stopWord)) {  
            stopWordLocated <- list.append(stopWordLocated, stopWord) 
          }         
          
          if (removeStopWords & stopWord %in% stopWordLocated) { 
            print(stopWord)

            instance$getData() %>>%
              {self$replaceStopWord(stopWord, .)} %>>%
                instance$setData()
          }  
        } 
             
        instance$addProperties(stopWordLocated,
                                super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(),
                                super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not an StopWordsJsonFile to apply to the language")
        warning(message)  
        instance$invalidate()
        return(NULL) 
      }
        
      return(instance)
    },

    findStopWord = function(data, stopWord) {
      
      if (!"character" %in% class(data)) {
        stop("[InterjectionFromStringBufferPipe][findInterjections][Error] 
                Checking the StopWordFromStringBufferPipe of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(stopWord)) {
        stop("[StopWordFromStringBufferPipe][findStopWord][Error] 
                Checking the type of the variable: stopWord ", 
                  class(stopWord))
      }               
      
      stopWord <- self$obtainStringEscaped(stopWord)
   
      #Usada en java
      #Pattern.compile( "(?:[\\p{Space}\\p{Punct}]|^)(" + 
      #Pattern.quote(((JsonString)v).getString()) + ")(?:[\\p{Space}\\p{Punct}]|$)" )

      #Revisar expresion regular usada la de interjeciones
      # regularExpresion <- paste0('(?:[\\p[:space:]]|^)(', 
      #                            stopWord,
      #                            ')(?:[\\p[:space:]]|$)'
      #                            , sep = "")
      regularExpresion <- paste0('(?:[\\p[:space:]\\p[:punct:]]|^)(', 
                                 stopWord,
                                 ')(?:[\\p[:space:]\\p[:punct:]]|$)',
                                 sep = "")
      return(grepl(pattern = regex(regularExpresion), x = data))
      
    },    
        
    replaceStopWord = function(stopWord, data) {
      
      if (!"character" %in% class(stopWord)) {
        stop("[StopWordFromStringBufferPipe][replaceStopWords][Error] 
                Checking the type of the variable: stopWord ", 
                  class(stopWord))
      }               
      
      
      if (!"character" %in% class(data)) {
        stop("[StopWordFromStringBufferPipe][replaceStopWords][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      stopWord <- self$obtainStringEscaped(stopWord)
      
      #Usada en java
      #Pattern.compile( "(?:[\\p{Space}\\p{Punct}]|^)(" + 
      #Pattern.quote(((JsonString)v).getString()) + ")(?:[\\p{Space}\\p{Punct}]|$)" )
      
      #Revisar expresion regular usada la de interjeciones
      regularExpresion <- paste0('(?:[\\p[:space:]\\p[:punct:]]|^)(', 
                                 stopWord,
                                 ')(?:[\\p[:space:]\\p[:punct:]]|$)',
                                 sep = "")
      
      return(gsub(regex(regularExpresion),
                  " ", data))
    },
    
    getPropertyLanguageName = function() {
      return(private$propertyLanguageName)
    },
    
    getPathResourcesStopWords = function() {
      return(private$pathResourcesStopWords)
    },
    
    obtainStringEscaped = function(string) {
      
      if (!"character" %in% class(string)) {
        stop("[StopWordFromStringBufferPipe][obtainStringEscaped][Error]
                Checking the type of the variable: string ", 
                  class(string))
      }  
      
      listCharacterToEscape <- list("\\\\", "\\.", "\\*" ,"\\^","\\$","\\?","\\(","\\)","\\[","\\]","\\+","\\{","\\}",'\\"',"\\'","\\|")
      
      for (ch in listCharacterToEscape) {
        string <- gsub(ch , paste0("\\", ch, sep = ""), string, perl = T)
        
      }
      
      return(string)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesStopWords = ""
  )
)
