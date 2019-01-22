GuessLanguageFromStringBufferPipe <- R6Class(
    
  "GuessLanguageFromStringBufferPipe",
 
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "language") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[GuessLanguageFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    }, 
    
    pipe = function(instance, languageTwitter = TRUE) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[GuessLanguageFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      if (languageTwitter 
            && instance$getSpecificProperty("extension") %in% "twtid") {

        if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                              "tweets/_", instance$getSpecificProperty("target"), "_/", 
                              instance$getId(), ".json", sep = ""))) {
          
          path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
                                instance$getSpecificProperty("target"), "_/", 
                                instance$getId(), ".json", sep = "")
          
          dataFromJsonFile <- fromJSON(file = path)
          
          if (!is.na(dataFromJsonFile[["lang"]]) && !is.null(dataFromJsonFile[["lang"]]) && dataFromJsonFile[["lang"]] != ""){
            
              instance$addProperties(dataFromJsonFile[["lang"]],super$getPropertyName())
            
              instance$addProperties(0,"Language score")
            
              instance$addProperties(0,"Language percent")
            
            return(instance)
          } 
        }
        
        
      } 
      
      
      instance$getData() %>>% 
        self$getLanguage() %>>%
          {instance$addProperties(.,super$getPropertyName())}
      
      instance$getData() %>>% 
        self$getLanguageScore() %>>%
          {instance$addProperties(.,"Language score")}
      
      instance$getData() %>>% 
        self$getLanguagePercent() %>>%
          {instance$addProperties(.,"Language percent")} 
         
      
      
      return(instance)
    },
    
    getLanguage = function(data) {
      
      if (!"character" %in% class(data)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguage][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(detectLanguage(data, isPlainText = TRUE)[[1]][[1]])
    },
    
    getLanguageScore = function(data) {

      if (!"character" %in% class(data)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguageScore][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(detectLanguage(data)[[7]])  
    },
    
    getLanguagePercent = function(data) {

      if (!"character" %in% class(data)) {
        stop("[GuessLanguageFromStringBufferPipe][getLanguagePercent][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(detectLanguage(data)[[10]])  
    }
  )  
)
