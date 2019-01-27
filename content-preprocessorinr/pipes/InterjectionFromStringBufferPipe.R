#Class to 
#
#
#Variables:
#
#
InterjectionFromStringBufferPipe <- R6Class(
  
  "InterjectionFromStringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "interjection",
                          propertyLanguageName = "language",
                          pathResourcesInterjections = "content-preprocessorinr/resources/interjections-json") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[InterjectionFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[InterjectionFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesInterjections)) {
        stop("[InterjectionFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesInterjections ", 
                  class(pathResourcesInterjections))
      }
      
      propertyName %>>% 
        super$initialize()
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesInterjections <- pathResourcesInterjections
      
    }, 
    
    pipe = function(instance, removeInterjections = TRUE) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[InterjectionFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }

      if (!"logical" %in% class(removeInterjections)) {
        stop("[FindUrlInStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeInterjections ", 
                  class(removeInterjections))
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
        return(instance)

      }
        
      if (file.exists(paste(self$getPathResourcesInterjections(),
                             "/interj.",
                               languageInstance,
                                 ".json",
                                    sep = ""))) {
        
        JsonFile <- paste(self$getPathResourcesInterjections(),
                            "/interj.",
                              languageInstance,
                                ".json",
                                  sep = "")  

        #Variable which stores the interjections located in the data
        interjectionsLocated <- list() 
      
        jsonData <- fromJSON(file = JsonFile)

        for (interjection in jsonData) {
          
          if (self$findInterjection(instance$getData(), interjection)) {  
            interjectionsLocated <- list.append(interjectionsLocated, interjection) 
          }
          
          if (removeInterjections && interjection %in% interjectionsLocated) {
            
            instance$getData() %>>%
              {self$replaceInterjection(interjection, .)} %>>%
                instance$setData()
          }  
        }     
        
        instance$addProperties(interjectionsLocated,
                                 super$getPropertyName())      
      } else {
        
        cat("There is not an interjectionsJsonFile to apply to the language: ", 
              languageInstance , 
                "\n")
        instance$addProperties(list(),
                                super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not an interjectionsJsonFile to apply to the language-> ", languageInstance)
        warning(message)  
        instance$invalidate()
        return(instance)
        
      }
      
            
      return(instance)
    },
    
    findInterjection = function(data, interjection) {

      if (!"character" %in% class(data)) {
        stop("[InterjectionFromStringBufferPipe][findInterjections][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(interjection)) {
        stop("[InterjectionFromStringBufferPipe][findInterjections][Error] 
                Checking the type of the variable: interjection ", 
                  class(interjection))
      }               
      
      interjection <- self$obtainStringEscaped(interjection)
      
      regularExpresion <- paste0('(?:[[:space:]]|^)?([¡]?(', 
                                 interjection,
                                 ')[!]?)(?=[[:space:]]|$)',
                                 sep = "")

      return(grepl(pattern = regex(regularExpresion), x = data, perl = T))
    },
    
    replaceInterjection = function(interjection, data) {
      
      if (!"character" %in% class(interjection)) {
        stop("[InterjectionFromStringBufferPipe][replaceInterjections][Error] 
                Checking the type of the variable: interjection ", 
                  class(interjection))
      }               
    
      
      if (!"character" %in% class(data)) {
        stop("[InterjectionFromStringBufferPipe][replaceInterjections][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      interjection <- self$obtainStringEscaped(interjection)
      
      regularExpresion <- paste0('(?:[[:space:]]|^)?([¡]?(', 
                                 interjection,
                                 ')[!]?)(?=[[:space:]]|$)',
                                 sep = "")
     
      return(gsub(regex(regularExpresion), "", data , perl = T))

    },
    
    getPropertyLanguageName = function() {
      return(private$propertyLanguageName)
    },
    
    getPathResourcesInterjections = function() {
      return(private$pathResourcesInterjections)
    },
    
    obtainStringEscaped = function(string) {
 
      if (!"character" %in% class(string)) {
        stop("[InterjectionFromStringBufferPipe][obtainStringEscaped][Error]
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
    pathResourcesInterjections = ""
  )
)
