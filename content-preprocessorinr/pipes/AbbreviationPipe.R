#Class to find and/or replace the abbreviations on the data
#
#First check if the instance has identified the language of the data. If there 
#is a source file associated with this language, the abbreviations in the data 
#are found and / or replaced.
#
#Variables:
#
#propertyLanguageName: (character) the name of property about language
#pathResourcesAbbreviations: (character) the path where are the resources
#
AbbreviationPipe <- R6Class(
  
  "AbbreviationPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "abbreviation", 
                          propertyLanguageName = "language",
                          pathResourcesAbbreviations = "content-preprocessorinr/resources/abbreviations-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated, 
      #and the place where the resources of the abbreviations are stored. 
      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   propertyLanguageName: (character) Name of the language property
      #   pathResourcesAbbreviations: (character) Path where are stored the 
      #                                           abbreviation resources
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #      
      
      if (!"character" %in% class(propertyName)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesAbbreviations)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesAbbreviations ", 
                  class(pathResourcesAbbreviations))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  Wclass(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesAbbreviations <- pathResourcesAbbreviations
    }, 
    
    pipe = function(instance, replaceAbbreviations = TRUE) {
      #
      #Function that preprocesses the instance to obtain/replace the abbreviations
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   replaceAbbreviations: (logical) indicate if the abbreviations are removed
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[AbbreviationPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(replaceAbbreviations)) {
        stop("[AbbreviationPipe][pipe][Error]
                Checking the type of the variable: replaceAbbreviations ", 
                  class(replaceAbbreviations))
      }  
      
      instance$addFlowPipes("AbbreviationPipe")
      
      if (!instance$checkCompatibility("AbbreviationPipe", self$getAlwaysBeforeDeps())) {
        stop("[AbbreviationPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
          
      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(),super$getPropertyName()) 
    
        cat("[AbbreviationPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() ," has not language property\n")
        
        return(instance)
      }
      
      JsonFile <- paste(self$getPathResourcesAbbreviations(),
                        "/abbrev.",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      #It is verified that there is a resource associated to the language of the instance
      if (!is.null(jsonData)) {
        
        #Variable which stores the abbreviations located in the data
        abbreviationsLocated <- list()           
        
        for (abbreviation in names(jsonData)) {

          if (self$findAbbreviation(instance$getData(), abbreviation)) {  
            abbreviationsLocated <- list.append(abbreviationsLocated, 
                                                  abbreviation) 
          }
          
          if (replaceAbbreviations && abbreviation %in% abbreviationsLocated) {
            
              instance$getData() %>>%
                {self$replaceAbbreviation(abbreviation, 
                                            as.character(jsonData[abbreviation]),
                                              .)} %>>%
                  trim() %>>%
                    instance$setData()
          }
        }     

        instance$addProperties(paste(abbreviationsLocated), super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(), super$getPropertyName()) 

        cat("[AbbreviationPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() , " has not an abbreviationsJsonFile ",
            "to apply to the language ->", languageInstance, " \n")
        
        return(instance)
      }


      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Abbreviation")
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[AbbreviationPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        return(instance)
      }
      
      return(instance)
    },

    findAbbreviation = function(data, abbreviation) {
      #
      #Function that checks if the abbreviation is in the data
      #
      #Args:
      #   data: (character) The text to preproccess
      #   abbreviation: (character) indicate the abbreviations to find
      #Returns:
      #   TRUE or FALSE depending on whether the abbreviation is on the data
      #   
      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][findAbbreviation][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationPipe][findAbbreviation][Error] 
                Checking the type of the variable: abbreviation ", 
                  class(abbreviation))
      }               

      abbreviationEscaped <- rex::escape(abbreviation)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(grepl(pattern = regex(regularExpresion), x = data, perl = T))
    },    
        
    replaceAbbreviation = function(abbreviation, extendedAbbreviation, data) {
      #
      #Function that replace the abbreviation in the data for the extendedAbbreviation
      #
      #Args:
      #   data: (character) The text to preproccess
      #   abbreviation: (character) indicate the abbreviation to remove
      #   extendedAbbreviation: (character) indicate the string to replace for the abbreviation
      #Returns:
      #   data with abbreviaton replaced
      #           
      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: abbreviation ", 
                  class(abbreviation))
      }               
        
      if (!"character" %in% class(extendedAbbreviation)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: extendedAbbreviation ", 
                  class(extendedAbbreviation))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }

      abbreviationEscaped <- rex::escape(abbreviation)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(gsub(regex(regularExpresion), 
                             paste(" ", extendedAbbreviation, " ", sep = ""), data, perl = T))
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
    
    getPathResourcesAbbreviations = function() {
      #
      #Getter of path of abbreviations resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pathResourcesAbbreviations variable
      #
      return(private$pathResourcesAbbreviations)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesAbbreviations = ""
  )
)
