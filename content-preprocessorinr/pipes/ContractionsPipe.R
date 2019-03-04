#Class to find and/or replace the contractions  on the data
#
#First check if the instance has identified the language of the data. If there 
#is a source file associated with this language, the contractions in the data 
#are found and / or replaced.
#
#Variables:
#
#propertyLanguageName: (character) the name of property about language
#pathResourcesAbbreviations: (character) tha path where are the resources
#
ContractionsPipe <- R6Class(
  
  "ContractionsPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "contractions", 
                          propertyLanguageName = "language",
                          pathResourcesContractions = "content-preprocessorinr/resources/contractions-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated, 
      #and the place where the resources of the contractions are stored. 
      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   propertyLanguageName: (character) Name of the language property
      #   pathResourcesContractions: (character) Path where are stored the 
      #                                          contractions resources
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #      
      
      if (!"character" %in% class(propertyName)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesContractions)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesContractions ", 
                  class(pathResourcesContractions))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesContractions <- pathResourcesContractions
    }, 
    
    pipe = function(instance, replaceContractions = TRUE) {
      #
      #Function that preprocesses the instance to obtain/replace the abbreviations
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #   replaceContractions: (logical) indicate if the contractions are replace
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[ContractionsPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(replaceContractions)) {
        stop("[ContractionsPipe][pipe][Error]
                Checking the type of the variable: replaceContractions ", 
                  class(replaceContractions))
      }  
      
      instance$addFlowPipes("ContractionsPipe")
      
      if (!instance$checkCompatibility("ContractionsPipe", self$getAlwaysBeforeDeps())) {
        stop("[ContractionsPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
      
      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) || 
          is.na(languageInstance) || 
          "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(),super$getPropertyName()) 
        
        cat("[ContractionsPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() ," has not language property\n")
        
        return(instance)
        
      }
      
      JsonFile <- paste(self$getPathResourcesContractions(),
                        "/contr.",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- resourceHandle$isLoadResource(JsonFile)
      
      #It is verified that there is a resource associated to the language of the instance
      if (!is.null(jsonData)) {
        
        #Variable which stores the contractions located in the data
        contractionsLocated <- list()           
        
        for (contraction in names(jsonData)) {
          
          if (self$findContraction(instance$getData(), contraction)) {  
            
            contractionsLocated <- list.append(contractionsLocated, 
                                               contraction) 
          }
          
          if (replaceContractions && contraction %in% contractionsLocated) {
            
            instance$getData() %>>%
              {self$replaceContraction(contraction, 
                                        as.character(jsonData[contraction]),
                                        .)} %>>%
                trim() %>>%
                  instance$setData()
          }
        }     
        
        instance$addProperties(paste(contractionsLocated),
                               super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(),super$getPropertyName()) 
 
        cat("[ContractionsPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() , " has not an contactionsJsonFile ",
            "to apply to the language ->", languageInstance, " \n")
        
        return(instance)
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Contractions")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[ContractionsPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findContraction = function(data, contraction) {
      #
      #Function that checks if the contraction is in the data
      #
      #Args:
      #   data: (character) instance to preprocces
      #   contraction: (character) indicate if the contraction are found
      #Returns:
      #   TRUE or FALSE depending on whether the contraction is on the data
      #   
      if (!"character" %in% class(data)) {
        stop("[ContractionsPipe][findContraction][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(contraction)) {
        stop("[ContractionsPipe][findContraction][Error] 
                Checking the type of the variable: contraction ", 
                  class(contraction))
      }               
      
      contractionEscaped <- rex::escape(contraction)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 contractionEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(grepl(pattern = regex(regularExpresion), x = data, perl = T, ignore.case = TRUE))
    },    
    
    replaceContraction = function(contraction, extendedContraction, data) {
      #
      #Function that replace the contraction in the data for the extendedContraction
      #
      #Args:
      #   data: (character) instance to preprocces
      #   contraction: (character) indicate the contraction to remove
      #   extendedContraction: (character) indicate the string to replace for the contraction
      #Returns:
      #   data with contaction replaced
      #           
      if (!"character" %in% class(contraction)) {
        stop("[ContractionsPipe][replaceContraction][Error] 
                Checking the type of the variable: contraction ", 
                  class(contraction))
      }               
      
      if (!"character" %in% class(extendedContraction)) {
        stop("[ContractionsPipe][replaceContraction][Error] 
                Checking the type of the variable: extendedContraction ", 
                  class(extendedContraction))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[ContractionsPipe][replaceContraction][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      contractionEscaped <- rex::escape(contraction)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 contractionEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(gsub(regex(regularExpresion), 
                  paste(" ", extendedContraction, " ", sep = ""), data, perl = T, ignore.case = TRUE))
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
    
    getPathResourcesContractions = function() {
      #
      #Getter of path of contractions resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pathResourcesContractions variable
      #
      return(private$pathResourcesContractions)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesContractions = ""
  )
)
