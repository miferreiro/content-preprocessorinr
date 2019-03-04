UnmatchedTextHandler <- R6Class(
  
  "UnmatchedTextHandler",
  
  public = list(
    
    initialize = function() {},
    
    handle = function(originalText, replacementText, lang) {
      stop("I'm an abstract interface method")
    }
  )
)