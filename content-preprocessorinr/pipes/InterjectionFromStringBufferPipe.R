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
    
    pipe = function(instance, removeInterjection = TRUE) {
      
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[InterjectionFromStringBufferPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }

      if (!"logical" %in% class(removeInterjection)) {
        stop("[FindUrlInStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeInterjection ", 
                  class(removeInterjection))
      }      
            
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
      
      if (is.null(languageInstance) || 
          is.na(languageInstance) || 
          "Unknown" %in% languageInstance) {
        #cat("languageInstance ", languageInstance,"\n")
        return(instance)
      }
        
      interjectionsJsonFiles <- list.files(path = self$getPathResourcesInterjections()
                                           ,recursive = TRUE
                                           ,full.names = TRUE
                                           ,all.files = TRUE)  
      
      JsonFile <- interjectionsJsonFiles[
                                      stri_detect_fixed(interjectionsJsonFiles, 
                                                        languageInstance)]

      #Variable which stores the interjections located in the data
      interjectionsLocated <- list() 
      
      if (length(JsonFile) == 1) {
        
        jsonData <- fromJSON(file = JsonFile)

        for (interjection in jsonData) {
          
          if ("f**k" %in% interjection) { next }
            
          if (self$findInterjection(instance$getData(), interjection)) {  
            interjectionsLocated <- list.append(interjectionsLocated, interjection) 
          }
          
          if (removeInterjection) {
            instance$getData() %>>%
              {self$replaceInterjection(interjection, .)} %>>%
                instance$setData()
          }  
        }     
        
        instance$addProperties(interjectionsLocated
                               , super$getPropertyName())      
      } else {
        
        cat("There is not an interjectionsJsonFile to apply to the language: " 
            , languageInstance , "\n")
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
      
      #Revisar expresion regular
      regularExpresion <- paste0('(?:[ ]+|^)([¡]?(', 
                                 interjection,
                                 ')[!]?)(?:[ ]+|$)'
                                 , sep = "")

      return(grepl(pattern = regularExpresion, x = data))
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
      
      #Revisar expresion regular
      regularExpresion <- paste0('(?:[ ]+|^)([¡]?(', 
                                 interjection,
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
    
    getPathResourcesInterjections = function(){
      return(private$pathResourcesInterjections)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesInterjections = ""
  )
)
