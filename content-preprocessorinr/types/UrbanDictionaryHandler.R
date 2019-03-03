UrbanDictionaryHandler <- R6Class(
  
  "UrbanDictionaryHandler",
  
  inherit = UnmatchedTextHandler,
  
  public = list(
    
    initialize = function() {},
    
    handle = function(originalText, replacementText, lang) {
      
      if (!"character" %in% class(originalText)) {
        stop("[UrbanDictionaryHandler][handle][Error] 
                Checking the type of the variable: originalText ", 
                  class(originalText))
      }
      
      if (!"character" %in% class(replacementText)) {
        stop("[UrbanDictionaryHandler][handle][Error] 
                Checking the type of the variable: replacementText ", 
                  class(replacementText))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][initialize][Error] 
                Checking the type of the variable: lang ", 
             class(lang))
      }
      
      
    }
  )
)