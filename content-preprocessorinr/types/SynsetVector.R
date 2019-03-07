#
#A class to represent a vector of synsets and the asociated information
# 
#Variables:
#
#originalText: (character) The original text
#fixedText: (character) The text after fixing unmatched text sections
#unmatchedTexts: (list) The list of detected synsets represented as Pairs where:
#                       - The name element is the synsetId identified by babelfy  
#                       - The value element is the portion of the fixedText that 
#                         matches the synsetId
#synsets: (list) The list of detected synsets represented as Pairs where: 
#                - The first element of the pair is the synsetId identified by 
#                  babelfy 
#                - The second element of the pair is the portion of the fixedText 
#                  that matches the synsetId
SynsetVector <- R6Class(
  
  "SynsetVector",
  
  public = list(
    
    initialize = function(originalText) {
      #
      #Class constructor initialize the variable of originalText.
      #
      #Args:
      #   originalText: (character) This is the original text parameter
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
      #
      #Setter of originalText variable
      #
      #Args:
      #   originalText: (character) the new value of originalText variable
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(originalText)) {
        stop("[SynsetVector][setOriginalText][Error]
                Checking the type of the variable: originalText ",
                  class(originalText))
      }
      
      private$originalText <- originalText
      
      return()
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
      #
      #Setter of fixedText variable
      #
      #Args:
      #   fixedText: (character) the new value of fixedText variable
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(fixedText)) {
        stop("[SynsetVector][setFixedText][Error]
                Checking the type of the variable: fixedText ",
                  class(fixedText))
      }
      
      private$fixedText <- fixedText
      
      return()
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
      #
      #Setter of unmatchedTexts variable
      #
      #Args:
      #   unmatchedTexts: (character) the new value of unmatchedTexts variable
      #
      #Returns:
      #   null
      #
      if (!"list" %in% class(unmatchedTexts)) {
        stop("[SynsetVector][setUnmatchedTexts][Error]
                Checking the type of the variable: unmatchedTexts ",
                  class(unmatchedTexts))
      }
      
      private$unmatchedTexts <- unmatchedTexts
      
      return()
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
      #
      #Setter of synsets variable
      #
      #Args:
      #   synsets: (list) the new value of synsets variable
      #
      #Returns:
      #   null
      #
      if (!"list" %in% class(synsets)) {
        stop("[SynsetVector][setSynsets][Error]
                Checking the type of the variable: synsets ",
                  class(synsets))
      }
      
      private$synsets <- synsets
      
      return()
    }
  ),
  private = list(
    originalText = "",
    fixedText = "",
    unmatchedTexts = list(),
    synsets = list()
  )
)