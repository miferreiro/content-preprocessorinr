BabelfyEntry <- R6Class(
  
  "BabelfyEntry",
  
  public = list(
    
    initialize = function(startIdx, endIdx, score, synsetId, text) {
      private$startIdx <- startIdx      
      private$endIdx <- endIdx
      private$score <- score
      private$synsetId <- synsetId
      private$text <- text
    },
    
    getStartIdx = function() {
      return(private$startIdx)
    },
    
    setStartIdx = function(startIdx) {
      private$startIdx <- startIdx
    },
    
    getEndIdx = function() {
      return(private$endIdx)
    },
    
    setEndIdx = function(endIdx) {
      private$endIdx <- endIdx
    },
    
    getScore = function() {
      return(private$score)
    },
    
    setScore = function(score) {
      private$score <- score
    },
    
    getSynsetId = function() {
      return(private$synsetId)
    },
    
    setSynsetId = function(synsetId) {
      private$synsetId <- synsetId
    },
    
    getText = function() {
      return(private$text)
    },
    
    setText = function(text) {
      private$text <- text
    }
  ),
  
  private = list(
    startIdx = 0,
    endIdx = 0,
    score = 0,
    synsetId = "",
    text = ""
  )
)