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

    initialize = function(propertyName = "abbreviation", 
                          propertyLanguageName = "language",
                          pathResourcesAbbreviations = "content-preprocessorinr/resources/abbreviations-json") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[AbbreviationFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[AbbreviationFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesAbbreviations)) {
        stop("[AbbreviationFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesAbbreviations ", 
                  class(pathResourcesAbbreviations))
      }
      
      
      propertyName %>>% 
        super$initialize()
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesAbbreviations <- pathResourcesAbbreviations
    }, 
    
    pipe = function(instance, removeAbbreviations = T) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[AbbreviationFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(removeAbbreviations)) {
        stop("[AbbreviationFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeAbbreviations ", 
                  class(removeAbbreviations))
      }  
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
          
      
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
      
      if (file.exists(paste(self$getPathResourcesAbbreviations(),
                       "/abbrev.",
                         languageInstance,
                           ".json",
                              sep = ""))) {
        
        JsonFile <- paste(self$getPathResourcesAbbreviations(),
                      "/abbrev.",
                        languageInstance,
                        ".json",sep = "")  

        #Variable which stores the abbreviations located in the data
        abbreviationsLocated <- list()           
        
        jsonData <- fromJSON(file = JsonFile)
      
        for (abbreviation in names(jsonData)) {

          if (self$findAbbreviation(instance$getData(), abbreviation)) {  
            abbreviationsLocated <- list.append(abbreviationsLocated, 
                                                  abbreviation) 
            print(abbreviation)
          }
          
          if (removeAbbreviations && abbreviation %in% abbreviationsLocated) {
            
              instance$getData() %>>%
                {self$replaceAbbreviation(abbreviation, 
                                            as.character(jsonData[abbreviation]),
                                              .)} %>>%
                  instance$setData()
          }
        }     

        instance$addProperties(abbreviationsLocated,
                                super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(),
                                super$getPropertyName()) 
        message <- c( "The file: " , instance$getPath() , " has not an abbreviationsJsonFile to apply to the language")
        warning(message)  
        instance$invalidate()
        return(NULL)
      }

      return(instance)
    },

    findAbbreviation = function(data, abbreviation) {
      
      if (!"character" %in% class(data)) {
        stop("[AbbreviationFromStringBufferPipe][findAbbreviation][Error] 
             Checking the type of the variable: data ", 
             class(data))
      }
      
      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationFromStringBufferPipe][findAbbreviation][Error] 
             Checking the type of the variable: abbreviation ", 
             class(abbreviation))
      }               

      abbreviation <- self$obtainStringEscaped(abbreviation)
      
      regularExpresion <- paste0('(?:[\\p[:space:]]|^)(', 
                                 abbreviation,
                                 ')(?:[\\p[:space:]]|$)',
                                 sep = "")
      
      return(grepl(pattern = regex(regularExpresion), x = data))
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

      abbreviation <- self$obtainStringEscaped(abbreviation)

      regularExpresion <- paste0('(?:[\\p[:space:]]|^)(', 
                                 abbreviation,
                                 ')(?:[\\p[:space:]]|$)',
                                 sep = "")

      return(gsub(regex(regularExpresion), 
                             paste0(" ",extendedAbbreviation," ",sep = ""), data))
    },
    
    getPropertyLanguageName = function() {
      return(private$propertyLanguageName)
    },
    
    getPathResourcesAbbreviations = function() {
      return(private$pathResourcesAbbreviations)
    },
    
    obtainStringEscaped = function(string) {
 
      if (!"character" %in% class(string)) {
        stop("[AbbreviationFromStringBufferPipe][obtainStringEscaped][Error]
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
    pathResourcesAbbreviations = ""
  )
)
