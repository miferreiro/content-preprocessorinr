#Class to find and/or replace the interjections on the data
#
#First check if the instance has identified the language of the data. If there 
#is a source file associated with this language, the interjections in the data 
#are found and / or replaced.
#
#Variables:
#
#propertyLanguageName: (character) the name of property about language
#pathResourcesInterjections: (character) tha path where are the resources
#
InterjectionFromStringBufferPipe <- R6Class(
  
  "InterjectionFromStringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "interjection",
                          propertyLanguageName = "language",
                          pathResourcesInterjections = "content-preprocessorinr/resources/interjections-json") {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated, 
      #and the place where the resources of the interjections are stored. 
      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   propertyLanguageName: (character) Name of the language property
      #   pathResourcesInterjections: (character) Path where are stored the interjections resources
      #Returns:
      #   null
      #     
           
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
      #
      #Function that preprocesses the instance to obtain/replace the interjections
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #   removeInterjections: (logical) indicate if the interjections are removed
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #   
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
      
      #It is verified that there is a resource associated to the language of the instance
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(),
                                super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not language property")

        warning(message)  

        return(instance)
      }
      
      JsonFile <- paste(self$getPathResourcesInterjections(),
                        "/interj.",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- resourceHandle$isLoadResource(JsonFile)
      
      if (!is.null(jsonData)) { 

        #Variable which stores the interjections located in the data
        interjectionsLocated <- list() 
      
        for (interjection in jsonData) {
          
          if (self$findInterjection(instance$getData(), interjection)) {  
            interjectionsLocated <- list.append(interjectionsLocated, interjection) 
          }
          
          if (removeInterjections && interjection %in% interjectionsLocated) {
            
            instance$getData() %>>%
              {self$removeInterjection(interjection, .)} %>>%
                instance$setData()
          }  
        }     
        
        instance$addProperties(paste(interjectionsLocated),
                                 super$getPropertyName())      
      } else {
        
        cat("There is not an interjectionsJsonFile to apply to the language: ", 
              languageInstance , 
                "\n")
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not an interjectionsJsonFile to apply to the language-> ", languageInstance)

        warning(message)  

        return(instance)
        
      }
      
      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Interjection")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
            
      return(instance)
    },
    
    findInterjection = function(data, interjection) {
      #
      #Function that checks if the interjection is in the data
      #
      #Args:
      #   data: (character) instance to preprocces
      #   interjection: (character) indicate if the interjecetion are removed
      #Returns:
      #   TRUE or FALSE depending on whether the interjection is on the data
      #   
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
      
      interjectionEscaped <- rex::escape(interjection)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)([¡]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(grepl(pattern = regex(regularExpresion), x = data, perl = T))
    },
    
    removeInterjection = function(interjection, data) {
      #
      #Function that remove the interjection in the data  
      #
      #Args:
      #   data: (character) instance to preprocces
      #   interjection: (character) indicate the interjection to remove
      #Returns:
      #   The data with interjection removed
      #        
      if (!"character" %in% class(interjection)) {
        stop("[InterjectionFromStringBufferPipe][removeInterjection][Error] 
                Checking the type of the variable: interjection ", 
                  class(interjection))
      }               
    
      
      if (!"character" %in% class(data)) {
        stop("[InterjectionFromStringBufferPipe][removeInterjection][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      interjectionEscaped <- rex::escape(interjection)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)([¡]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
     
      return(gsub(regex(regularExpresion), "", data , perl = T))

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
    
    getPathResourcesInterjections = function() {
      #
      #Getter of path of interjections resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pathResourcesInterjections variable
      #
      return(private$pathResourcesInterjections)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesInterjections = ""
  )
)
