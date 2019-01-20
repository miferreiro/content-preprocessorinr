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
        
    pipe = function(instance, removeStopWord = TRUE) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[StopWordFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }

      if (!"logical" %in% class(removeStopWord)) {
        stop("[StopWordFromStringBufferPipe][pipe][Error]
             Checking the type of the variable: removeStopWord ", 
             class(removeStopWord))
      }    
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
      
      if (is.null(languageInstance) || 
          is.na(languageInstance) || 
          "Unknown" %in% languageInstance) {
        #cat("languageInstance ", languageInstance,"\n")
        return(instance)
      }      

      stopWordsJsonFiles <- list.files(path = self$getPathResourcesStopWords()
                                           ,recursive = TRUE
                                           ,full.names = TRUE
                                           ,all.files = TRUE)  
      
      JsonFile <- stopWordsJsonFiles[stri_detect_fixed(stopWordsJsonFiles, 
                                                        languageInstance)]
      
      stopWordLocated <- list()
      
      if (length(JsonFile) == 1) {
        
        jsonData <- fromJSON(file = JsonFile)
        
        for (stopWord in jsonData) {
          
          if (self$findStopWord(instance$getData(), stopWord)) {  
            stopWordLocated <- list.append(stopWordLocated, stopWord) 
          }         
          
          if (removeStopWord) {          
            instance$getData() %>>%
              {self$replaceStopWord(stopWord, .)} %>>%
                instance$setData()
          }  
        } 
             
        instance$addProperties(stopWordLocated
                               , super$getPropertyName()) 
        
      } else {
        
        cat("There is not an stopWordsJsonFiles to apply to the language: " 
            , languageInstance , "\n")
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
      
      #Usada en java
      #Pattern.compile( "(?:[\\p{Space}\\p{Punct}]|^)(" + 
      #Pattern.quote(((JsonString)v).getString()) + ")(?:[\\p{Space}\\p{Punct}]|$)" )

      #Revisar expresion regular usada la de interjeciones
      regularExpresion <- paste0('(?:[ ]+|^)([¡]?(', 
                                 stopWord,
                                 ')[!]?)(?:[ ]+|$)'
                                 , sep = "")
      
      return(grepl(pattern = regularExpresion, x = data))
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
      
      #Usada en java
      #Pattern.compile( "(?:[\\p{Space}\\p{Punct}]|^)(" + 
      #Pattern.quote(((JsonString)v).getString()) + ")(?:[\\p{Space}\\p{Punct}]|$)" )
      
      #Revisar expresion regular usada la de interjeciones
      regularExpresion <- paste0('(?:[ ]+|^)([¡]?(', 
                                  stopWord,
                                 ')[!]?)(?:[ ]+|$)'
                                 , sep = "")
      
      #print(regularExpresion)
      return(str_replace_all(data,
                             regex(regularExpresion), 
                             " "))
      
    },
    
    getPropertyLanguageName = function() {
      return(private$propertyLanguageName)
    },
    
    getPathResourcesStopWords = function(){
      return(private$pathResourcesStopWords)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesStopWords = ""
  )
)
