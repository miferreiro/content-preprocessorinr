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
    
    initialize = function(propertyName = "") {
      
      if (!"character" %in% class(propertyName)) {
        stop("[StripHTMLFromStringBufferPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      propertyName %>>% 
        super$initialize()
    },  
    
    pipe = function(instance) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[StripHTMLFromStringBufferPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }

      instance$getData() %>>% 
        self$getDataWithOutHtml() %>>%
          instance$setData()
        

      return(instance);
    },
    
    getDataWithOutHtml = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[StripHTMLFromStringBufferPipe][getDataWithOutHtml][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
   
      encoding <- guess_encoding(data)[1,1]
      print(encoding)
      decoded <- HTMLdecode(data)
      print(encoding)
      
      doc <- XML::htmlParse(data ,encoding = encoding, asText = TRUE)
      plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      plain.text2 <- paste0(plain.text, collapse = "") 
      
      plain.text3 <- self$cleanText(plain.text2)
      
      return(plain.text3)
      # return( data %>>% replace_html() )
    },
    
    cleanText = function(plainText) {
      
      plainText <- str_replace_all(plainText,"[[:space:] ]+"," ")
      plainText <- str_replace_all(plainText,fixed("\t"),"")
      plainText <- str_replace_all(plainText,fixed("\n"),"")
      
      return(plainText)
    }
    
  )  
)
