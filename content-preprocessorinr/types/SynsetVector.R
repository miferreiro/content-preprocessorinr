#
#Variables:
#
#propertyName: 
#
SynsetVector <- R6Class(
  
  "SynsetVector",
  
  public = list(
    
    initialize = function(originalText) {
      #
      #Class constructor
      #

      #
      #Args:
      #  
      #
      #Returns:
      #   null
      #      
      if (!"character" %in% class(originalText)) {
        stop("[SynsetVector][initialize][Error] 
             Checking the type of the variable: originalText ", 
             class(originalText))
      }

      private$originalText <- originalText
    },
    
    getOriginalText = function() {
      #
      #Getter of originalText
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of originalText variable
      #
      return(private$originalText)
    },
    
    setOriginalText = function(originalText) {
      private$originalText <- originalText
    },
    
    getFixedText = function() {
      #
      #Getter of fixedText
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of fixedText variable
      #
      return(private$fixedText)
    },
    
    setFixedText = function(fixedText) {
      private$fixedText <- fixedText
    },
    
    getUnmatchedTexts = function() {
      #
      #Getter of unmatchedTexts
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of unmatchedTexts variable
      #
      return(private$unmatchedTexts)
    },
    
    setUnmatchedTexts = function(unmatchedTexts) {
      private$unmatchedTexts <- unmatchedTexts
    },
    
    getSynsets = function() {
      #
      #Getter of synsets
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of synsets variable
      #
      return(private$synsets)
    },
    
    setSynsets = function(synsets) {
      private$synsets <- synsets
    }
  ),
  
  private = list(
    # 
    # The original text
    # 
    originalText = "",
    # 
    # The text after fixing unmatched text sections
    # 
    fixedText = "",
    # 
    # The vector of detected synsets represented as Pairs where: + The name
    # element is the synsetId identified by babelfy + The value
    # element is the portion of the fixedText that matches the synsetId
    # 
    unmatchedTexts = list(),
    
    # 
    # The vector of detected synsets represented as Pairs where: + The first
    # element of the pair is the synsetId identified by babelfy + The second
    # element of the pair is the portion of the fixedText that matches the
    # synsetId
    # 
    synsets = list()
  )
)
