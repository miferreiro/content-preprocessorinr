#Class to find and/or replace the slangs on the data
#
#First check if the instance has identified the language of the data. If there 
#is a source file associated with this language, the slangs in the data 
#are found and / or replaced.
#
#Variables:
#
#propertyLanguageName: (character) the name of property about language
#pathResourcesSlangs: (character) tha path where are the resources
#
SlangPipe <- R6Class(
  
  "SlangPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "langpropname", 
                          propertyLanguageName = "language",
                          pathResourcesSlangs = "content-preprocessorinr/resources/slangs-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated, 
      #and the place where the resources of the slangs are stored. 
      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   propertyLanguageName: (character) Name of the language property
      #   pathResourcesSlangs: (character) Path where are stored the slangs resources
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #           
      if (!"character" %in% class(propertyName)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesSlangs)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesSlangs ", 
                  class(pathResourcesSlangs))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesSlangs <- pathResourcesSlangs
    }, 
    
    pipe = function(instance, replaceSlangs = TRUE) {
      #
      #Function that preprocesses the instance to obtain/replace the slangs
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   removeSlangs: (logical) indicate if the slangs are removed
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #            
      if (!"Instance" %in% class(instance)) {
        stop("[SlangPipe][pipe][Error]
               Checking the type of the variable: instance ", 
                class(instance))
      }
      
      if (!"logical" %in% class(replaceSlangs)) {
        stop("[SlangPipe][pipe][Error]
                Checking the type of the variable: replaceSlangs ", 
                  class(replaceSlangs))
      }  
      
      instance$addFlowPipes("SlangPipe")
      
      if (!instance$checkCompatibility("SlangPipe", self$getAlwaysBeforeDeps())) {
        stop("[SlangPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      #It is verified that there is a resource associated to the language of the instance
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: ", instance$getPath(), " has not language property")
        
        cat("[SlangPipe][pipe][Warning] ", message, " \n")

        return(instance)
      }
      
      JsonFile <- paste(self$getPathResourcesSlangs(),
                        "/slang.",
                        languageInstance,
                        ".json",
                        sep = "") 
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      if (!is.null(jsonData)) { 
        
        #Variable which stores the Slangs located in the data
        slangsLocated <- list()           
        
        for (slang in names(jsonData)) {
          
          if (self$findSlang(instance$getData(), slang)) {  
            slangsLocated <- list.append(slangsLocated, slang) 
          }
          
          if (replaceSlangs && slang %in% slangsLocated) {
            instance$getData() %>>%
              {self$replaceSlang(slang, as.character(jsonData[slang]), .)} %>>%
                trim() %>>%
                  instance$setData()
          }
        }     
        
        instance$addProperties(paste(slangsLocated), super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: ", instance$getPath(), " has not an SlangsJsonFile to apply to the language-> ", languageInstance )
       
        cat("[SlangPipe][pipe][Warning] ", message, " \n")
        

        return(instance)
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: ", instance$getPath(), " has data empty on pipe Slang")
        
        instance$addProperties(message, "reasonToInvalidate")   

        cat("[SlangPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findSlang = function(data, slang) {
      #
      #Function that checks if the slang is in the data
      #
      #Args:
      #   data: (character) The text to preproccess
      #   slang: (character) indicate the slang to find
      #Returns:
      #   TRUE or FALSE depending on whether the slang is on the data
      #         
      if (!"character" %in% class(data)) {
        stop("[SlangPipe][findSlang][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(slang)) {
        stop("[SlangPipe][findSlang][Error] 
                Checking the type of the variable: slang ", 
                  class(slang))
      }               
      
      slangEscaped <- rex::escape(slang)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.']|^)(", 
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")
    
      return(grepl(pattern = regex(regularExpresion), x = data, perl = T, ignore.case = TRUE))
    },    
    
    replaceSlang = function(slang, extendedSlang, data) {
      #
      #Function that replace the slang in the data for the extendedSlang
      #
      #Args:
      #   data: (character) The text to preproccess
      #   slang: (character) indicate the slang to remove
      #   extendedSlang: (character) indicate the string to replace for the slang
      #Returns:
      #   data with slang replaced
      #         
      if (!"character" %in% class(slang)) {
        stop("[SlangPipe][replaceSlang][Error] 
                Checking the type of the variable: slang ", 
                  class(abbreviation))
      }               
      
      if (!"character" %in% class(extendedSlang)) {
        stop("[SlangPipe][replaceSlang][Error] 
                Checking the type of the variable: extendedSlang ", 
                  class(extendedSlang))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[SlangPipe][replaceSlang][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }

      slangEscaped <- rex::escape(slang)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.']|^)(", 
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(gsub(regex(regularExpresion), 
                          paste(" ", extendedSlang, " ", sep = ""), data, perl = T, ignore.case = TRUE))
      
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
    
    getPathResourcesSlangs = function() {
      #
      #Getter of path of slangs resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pathResourcesSlangs variable
      #
      return(private$pathResourcesSlangs)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesSlangs = ""
  )
)
