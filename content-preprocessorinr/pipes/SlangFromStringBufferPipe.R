#
#
#
#Variables:
#
#
SlangFromStringBufferPipe <- R6Class(
  
  "SlangFromStringBufferPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "langpropname", 
                          propertyLanguageName = "language",
                          pathResourcesSlangs = "content-preprocessorinr/resources/slangs-json") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[SlangFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[SlangFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesSlangs)) {
        stop("[SlangFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesSlangs ", 
                  class(pathResourcesSlangs))
      }
      
      
      propertyName %>>% 
        super$initialize()
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesSlangs <- pathResourcesSlangs
    }, 
    
    pipe = function(instance, removeSlangs = TRUE) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[SlangFromStringBufferPipe][pipe][Error]
             Checking the type of the variable: instance ", 
             class(instance))
      }
      
      if (!"logical" %in% class(removeSlangs)) {
        stop("[SlangFromStringBufferPipe][pipe][Error]
             Checking the type of the variable: removeSlangs ", 
             class(removeSlangs))
      }  
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      
      if (is.null(languageInstance) || 
          is.na(languageInstance) || 
          "Unknown" %in% languageInstance) {
        #cat("languageInstance ", languageInstance,"\n")
        instance$addProperties(list()
                               , super$getPropertyName()) 
        return(instance)
      }
      
      slangsJsonFiles <- list.files(path = self$getPathResourcesSlangs()
                                           ,recursive = TRUE
                                           ,full.names = TRUE
                                           ,all.files = TRUE)  
      
      JsonFile <- slangsJsonFiles[
        stri_detect_fixed(slangsJsonFiles, 
                          languageInstance)]
      
      #Variable which stores the Slangs located in the data
      slangsLocated <- list()           
      
      if (length(JsonFile) == 1) {
        
        jsonData <- fromJSON(file = JsonFile)
        
        for (slang in names(jsonData)) {
          
          if (self$findSlang(instance$getData(), slang)) {  
            slangsLocated <- list.append(slangsLocated, slang) 
          }
          
          if (removeSlangs & slang %in% slangsLocated) {
            instance$getData() %>>%
              {self$replaceSlang(slang, as.character(jsonData[slang]), .)} %>>%
                instance$setData()
          }
        }     
        
        instance$addProperties(slangsLocated
                               , super$getPropertyName()) 
        
      } else {
        
        cat("There is not an SlangsJsonFile to apply to the language: " 
            , languageInstance , "\n")
        instance$addProperties(list()
                               , super$getPropertyName()) 
      }
      
      return(instance)
    },
    
    findSlang = function(data, slang) {
      
      if (!"character" %in% class(data)) {
        stop("[SlangFromStringBufferPipe][findSlang][Error] 
             Checking the type of the variable: data ", 
             class(data))
      }
      
      if (!"character" %in% class(slang)) {
        stop("[SlangFromStringBufferPipe][findSlang][Error] 
             Checking the type of the variable: slang ", 
             class(slang))
      }               
      
      slang <- self$obtainStringEscaped(slang)
      
      
      #Revisar expresion regular
      regularExpresion <- paste0('(?:[ ]+|^)(', 
                                 slang,
                                 ')(?:[ ]+|$)'
                                 , sep = "")
      
      # return(str_extract_all(data,
      #                        regex(regularExpresion)))
      
      return(grepl(pattern = regex(regularExpresion), x = data))
    },    
    
    replaceSlang = function(slang, extendedSlang, data) {
      
      if (!"character" %in% class(slang)) {
        stop("[SlangFromStringBufferPipe][replaceSlang][Error] 
             Checking the type of the variable: slang ", 
             class(abbreviation))
      }               
      
      if (!"character" %in% class(extendedSlang)) {
        stop("[SlangFromStringBufferPipe][replaceSlang][Error] 
             Checking the type of the variable: extendedSlang ", 
             class(extendedSlang))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[SlangFromStringBufferPipe][replaceSlang][Error] 
             Checking the type of the variable: data ", 
             class(data))
      }
      
      slang <- self$obtainStringEscaped(slang)
      
      
      #Revisar expresion regular
      regularExpresion <- paste0('(?:[ ]+|^)(', 
                                 slang,
                                 ')(?:[ ]+|$)'
                                 , sep = "")
      
      #print(regularExpresion)
      return(str_replace_all(data,
                             regex(regularExpresion), 
                             paste0(" ",extendedSlang," ",sep = "")))
      
    },
    
    getPropertyLanguageName = function() {
      return(private$propertyLanguageName)
    },
    
    getPathResourcesSlangs = function(){
      return(private$pathResourcesSlangs)
    },
    
    obtainStringEscaped = function(string) {
      
      listCharacterToEscape <- list("\\\\", "\\.", "\\*" ,"\\^","\\$","\\?","\\(","\\)","\\[","\\]","\\+","\\{","\\}",'\\"',"\\'","\\|")
      
      for (ch in listCharacterToEscape) {
        string <- gsub(ch , paste0("\\", ch, sep = ""), string, perl = T)
        
      }
      
      return(string)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesSlangs = ""
  )
)
