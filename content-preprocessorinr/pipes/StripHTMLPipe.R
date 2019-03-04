#Class to remove html tags
#
#Variables:
#
StripHTMLPipe <- R6Class(
    
  "StripHTMLPipe",

  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
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
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #           
      if (!"character" %in% class(propertyName)) {
        stop("[StripHTMLPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StripHTMLPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StripHTMLPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },  
    
    pipe = function(instance) {
      #
      #Function that preprocesses the instance to remove html tags
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[StripHTMLPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      instance$addFlowPipes("StripHTMLPipe")
      
      if (!instance$checkCompatibility("StripHTMLPipe", self$getAlwaysBeforeDeps())) {
        stop("[StripHTMLPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getData() %>>% 
        self$getDataWithOutHtml() %>>%
          trim() %>>%
            instance$setData()
        
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StripHTML")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[StripHTMLPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance);
    },
    
    getDataWithOutHtml = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[StripHTMLPipe][getDataWithOutHtml][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
    
      doc <- XML::htmlParse(data ,encoding = "UTF-8", asText = TRUE)
      plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      plain.text2 <- paste0(plain.text, collapse = "") 
      plain.text3 <- self$cleanText(plain.text2)
      
      return(plain.text3)
    },
    
    cleanText = function(plainText) {
      #
      #Function to remove \t,\n and spaces from the text
      #
      #Args:
      #   plainText: (character) text to remove \t,\n and spaces
      #Returns:
      #   The text without \t,\n and spaces
      #   
      
      if (!"character" %in% class(plainText)) {
        stop("[StripHTMLPipe][cleanText][Error] 
                Checking the type of the variable: plainText ", 
                  class(plainText))
      }
      
      plainText <- gsub("\\\\t", " ", plainText)
      plainText <- gsub("\\\\n", " ", plainText)
      plainText <- gsub("[[:space:]]+", " ", plainText)
      
      return(plainText)
    }
  )  
)
