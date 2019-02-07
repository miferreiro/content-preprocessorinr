#Class to 
#
#
#Variables:
#
#
StripHTMLFromStringBufferPipe <- R6Class(
    
  "StripHTMLFromStringBufferPipe",

  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StripHTMLFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },  
    
    pipe = function(instance) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[StripHTMLFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "StripHTMLFromStringBufferPipe")
      
      if (!super$checkCompatibility("StripHTMLFromStringBufferPipe")) {
        stop("[StripHTMLFromStringBufferPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      
      instance$getData() %>>% 
        self$getDataWithOutHtml() %>>%
          instance$setData()
        
      if (is.na(instance$getData()) || all(instance$getData() == "") || is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StripHTML")
        instance$addProperties(message, "reasonToInvalidate")   
        warning(message)  
        
        instance$invalidate()
        return(instance)
      }
      
      return(instance);
    },
    
    getDataWithOutHtml = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[StripHTMLFromStringBufferPipe][getDataWithOutHtml][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
    
      # Encoding(data) <- "UTF-8"
      # encoding <- guess_encoding(path)[1,1]
      # encoding <- unlist(stri_enc_detect(data))[1]
      # # print(encoding)
      # decoded <- HTMLdecode(data)
      # # print("decoded")
      # # print(decoded)
      # encoding <- unlist(stri_enc_detect(decoded))[1]
      # # 
      # print("enconding")
      # print(encoding)
      
      doc <- XML::htmlParse(data ,encoding = "UTF-8", asText = TRUE)
      plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      plain.text2 <- paste0(plain.text, collapse = "") 
      plain.text3 <- self$cleanText(plain.text2)
      
      return(plain.text3)

    },
    
    cleanText = function(plainText) {
      
      plainText <- gsub("\\\\t", " ", plainText)
      plainText <- gsub("\\\\n", " ", plainText)
      plainText <- gsub("[[:space:]]+", " ", plainText)
      
      return(plainText)
    }
    
  )  
)
