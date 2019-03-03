UnmatchedTextHandler <- R6Class(
  
  "UnmatchedTextHandler",
  
  public = list(
    
    initialize = function() {},
    
    handle = function(text, lang) {
      stop("I'm an abstract interface method")
    }
  )
)