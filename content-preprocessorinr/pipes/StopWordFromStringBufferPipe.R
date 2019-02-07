#Class to find and/or replace the stopwords on the data
#
#First check if the instance has identified the language of the data. If there 
#is a source file associated with this language, the stopwords in the data 
#are found and / or replaced.
#
#Variables:
#
#propertyLanguageName: (character) the name of property about language
#pathResourcesStopWords: (character) tha path where are the resources
#
StopWordFromStringBufferPipe <- R6Class(
  
  "StopWordFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "stopWord",
                          propertyLanguageName = "language",
                          pathResourcesStopWords = "content-preprocessorinr/resources/stopwords-json",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated, 
      #and the place where the resources of the stopwords are stored. 
      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   propertyLanguageName: (character) Name of the language property
      #   pathResourcesStopWords: (character) Path where are stored the stopwords resources
      #Returns:
      #   null
      #            
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
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StopWordFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StopWordFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesStopWords <- pathResourcesStopWords
    }, 
        
    pipe = function(instance, removeStopWords = TRUE) {
      #
      #Function that preprocesses the instance to obtain/replace the stopwords
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #   removeStopWords: (logical) indicate if the stopwprds are removed
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #         
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
      
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "StopWordFromStringBufferPipe")
      
      if (!super$checkCompatibility("StopWordFromStringBufferPipe")) {
        stop("[StopWordFromStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <-
          paste("The file: " ,
                instance$getPath() ,
                " has not language property",
                sep = "")
        
        warning(message)  
        
        return(instance)
      }      

      JsonFile <- paste(self$getPathResourcesStopWords(),
                        "/",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- resourceHandle$isLoadResource(JsonFile)
      
      if (!is.null(jsonData)) { 
        
        #Variable which stores the stopwords located in the data
        stopWordLocated <- list()
      
        for (stopWord in jsonData) {
          
          if (self$findStopWord(instance$getData(), stopWord)) {  
            stopWordLocated <- list.append(stopWordLocated, stopWord) 
          }         
          
          if (removeStopWords && stopWord %in% stopWordLocated) { 

            instance$getData() %>>%
              {self$removeStopWord(stopWord, .)} %>>%
                instance$setData()
          }  
        } 
             
        instance$addProperties(paste(stopWordLocated),
                                super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <-
          paste(
            "The file: " ,
            instance$getPath() ,
            " has not an StopWordsJsonFile to apply to the language-> ",
            languageInstance,
            sep = ""
          )

        warning(message)  
        
        return(instance) 
      }
      
      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StopWord")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
        
      return(instance)
    },

    findStopWord = function(data, stopWord) {
      #
      #Function that checks if the stopword is in the data
      #
      #Args:
      #   data: (character) instance to preprocces
      #   stopWord: (character) indicate if the stopWord are removed
      #Returns:
      #   TRUE or FALSE depending on whether the stopWord is on the data
      #         
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
      
      stopWordEscaped <- rex::escape(stopWord)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(" , 
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(grepl(pattern = regex(regularExpresion), x = data , perl = T))
      
    },    
        
    removeStopWord = function(stopWord, data) {
      #
      #Function that remove the stopword in the data  
      #
      #Args:
      #   data: (character) instance to preprocces
      #   stopWord: (character) indicate the stopWord to removed
      #Returns:
      #   The data with stopwords removed
      #          
      if (!"character" %in% class(stopWord)) {
        stop("[StopWordFromStringBufferPipe][removeStopWord][Error] 
                Checking the type of the variable: stopWord ", 
                  class(stopWord))
      }               
      
      if (!"character" %in% class(data)) {
        stop("[StopWordFromStringBufferPipe][removeStopWord][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      stopWordEscaped <- rex::escape(stopWord)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(" , 
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(gsub(regex(regularExpresion),"", data, perl = T))
    },
    
    getPropertyLanguageName = function() {
      #
      #Getter of name of property language
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of propertyLanguageName variable
      #
      return(private$propertyLanguageName)
    },
    
    getPathResourcesStopWords = function() {
      #
      #Getter of path of stopwords resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pathResourcesStopWords variable
      #
      return(private$pathResourcesStopWords)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesStopWords = ""
  )
)
