#Class to drops stopwords from texts The data of the instance should contain 
#a text without HTML Tags
#
#Variables:
#
#
AbbreviationFromStringBufferPipe <- R6Class(
  
  "AbbreviationFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "", 
                          propertyLanguageName = "language",
                          pathResourcesAbreviations = "content-preprocessorinr/resources/abbreviations-json") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[AbbreviationFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesAbreviations <- pathResourcesAbreviations
    }, 
    
    pipe = function(instance) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[AbbreviationFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
          
      
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        cat("languageInstance ", languageInstance,"\n")
        return(instance)
      }
      
      abbreviationsJsonFiles <- list.files(path = self$getPathResourcesAbreviations()
                                          ,recursive = TRUE
                                          ,full.names = TRUE
                                          ,all.files = TRUE)  
   
      JsonFile <- abbreviationsJsonFiles[
                                       stri_detect_fixed(abbreviationsJsonFiles, 
                                                         languageInstance)]
      
      cat("JsonFile " , JsonFile,"\n")                   
      
      if (length(JsonFile) == 1) {
        
        jsonData <- fromJSON(file = JsonFile)
        print(instance$getData())
        for (abbreviation in names(jsonData)) {
          #print(abbreviation)
          #print(jsonData[abbreviation])
          instance$getData() %>>%
            {self$replaceAbbreviation(abbreviation, as.character(jsonData[abbreviation]), .)} %>>%
              instance$setData()
        }     
        
        print(instance$getData())
        Sys.sleep(1)
      } else {
        
        cat("There is not an abbreviationsJsonFile to apply to the language: " 
              , languageInstance , "\n")
      }
      #Unknown
      
      
      
      return(instance)
    },
    
    replaceAbbreviation = function(abbreviation, extendedAbbreviation, data) {
        
      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationFromStringBufferPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: abbreviation ", 
                  class(abbreviation))
      }               
        
      if (!"character" %in% class(extendedAbbreviation)) {
        stop("[AbbreviationFromStringBufferPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: extendedAbbreviation ", 
                  class(extendedAbbreviation))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[AbbreviationFromStringBufferPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      regularExpresion <- paste0("(?:[\\p{Space}]|^)(", 
                                 abbreviation, 
                                 ")(?:[\\p{Space}]|$)" 
                                 , sep = "")
      
      return(str_replace_all(data,
                              regex(regularExpresion), extendedAbbreviation))
    },
    
    getPropertyLanguageName = function() {
      return(private$propertyLanguageName)
    },
    
    getPathResourcesAbreviations = function(){
      return(private$pathResourcesAbreviations)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesAbreviations = ""
  )
)
