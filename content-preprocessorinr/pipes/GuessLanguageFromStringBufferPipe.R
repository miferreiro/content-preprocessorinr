GuessLanguageFromStringBufferPipe <- R6Class(
    
  "GuessLanguageFromStringBufferPipe",
 
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "language",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[GuessLanguageFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[GuessLanguageFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[GuessLanguageFromStringBufferPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    }, 
    
    pipe = function(instance, languageTwitter = TRUE) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[GuessLanguageFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "GuessLanguageFromStringBufferPipe")
      
      if (!super$checkCompatibility("GuessLanguageFromStringBufferPipe")) {
        stop("[GuessLanguageFromStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      if (languageTwitter 
            && instance$getSpecificProperty("extension") %in% "twtid") {

        if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                                "tweets/_", 
                                  instance$getSpecificProperty("target"), 
                                    "_/", 
                                      instance$getId(), 
                                        ".json", 
                                          sep = ""))) {
          
          path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
                          instance$getSpecificProperty("target"), 
                            "_/", 
                              instance$getId(), 
                                ".json", 
                                  sep = "")
          
          dataFromJsonFile <- fromJSON(file = path)
          
          if (!is.na(dataFromJsonFile[["lang"]]) && 
                !is.null(dataFromJsonFile[["lang"]]) 
                  && dataFromJsonFile[["lang"]] != "") {
            
            langTwitter <- dataFromJsonFile[["lang"]]
            
            instance$addProperties(langTwitter,super$getPropertyName())
            
            if (is.null(instance$getSpecificProperty("language"))) {
              message <- c( "The file: " , instance$getPath() , " has a NULL twitter language")
              warning(message)    
              instance$addProperties(message, "reasonToInvalidate") 
              instance$invalidate()
              return(instance)
            }
          
            return(instance)
          } 
        }
      } 
      
      instance$getData() %>>% 
        self$getLanguage() %>>%
          {instance$addProperties(.,super$getPropertyName())}
      
      if ( is.null(instance$getSpecificProperty("language"))) {
        message <- c( "The file: " , instance$getPath() , " has a null language")
        warning(message)    
        instance$addProperties(message, "reasonToInvalidate") 
        instance$invalidate()
        return(instance)
      }
      
      return(instance)
    },
    
    getLanguage = function(data) {

      if (!"character" %in% class(data)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguage][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      langStandardize <- detect_language(data, plain_text = TRUE)

      return(langStandardize)
    }
  )  
)
