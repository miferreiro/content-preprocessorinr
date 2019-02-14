#Class to guess the language by using language detector of library cld2
#
#Variables:
#
# 
GuessLanguageFromStringBufferPipe <- R6Class(
    
  "GuessLanguageFromStringBufferPipe",
 
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "language",  
                          alwaysBeforeDeps = list("StoreFileExtensionPipe", 
                                                  "TargetAssigningFromPathPipe",
                                                  "StripHTMLFromStringBufferPipe"), 
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
      #
      #Function that preprocesses the instance to obtain the language of the data
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   languageTwitter: (logical) indicates whether for the instances of type 
      #                              twtid the language that returns the api is 
      #                              obtained or the detector is applied
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #             
      if (!"Instance" %in% class(instance)) {
        stop("[GuessLanguageFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      TypePipe[["private_fields"]][["flowPipes"]] <- 
        list.append(TypePipe[["private_fields"]][["flowPipes"]], "GuessLanguageFromStringBufferPipe")
      
      if (!super$checkCompatibility("GuessLanguageFromStringBufferPipe")) {
        stop("[GuessLanguageFromStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
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
              
              instance$addProperties(message, "reasonToInvalidate") 
              
              cat("[GuessLanguageFromStringBufferPipe][pipe][Warning] ", message, " \n")
              
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
      
      if (is.na(instance$getSpecificProperty("language"))) {
        message <- c( "The file: " , instance$getPath() , " has a null language")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[GuessLanguageFromStringBufferPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    getLanguage = function(data) {
      #
      #Function that guess the language of data
      #
      #Args:
      #   data: (character) text to guess the language
      #Returns:
      #   The language guesser. Format: see ISO 639-3:2007
      #   
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
