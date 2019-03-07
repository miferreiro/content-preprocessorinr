#
#A class to represent a vector of synset-based features 
# 
#Variables:
#
#synsetFeature: (list) A map of synsets together with its values
SynsetFeatureVector <- R6Class(
  
  "SynsetFeatureVector",
  
  public = list(
    
    initialize = function(synsetFeature) {
      #
      #Class constructor initialize a SynsetFeatureVector from a list where the 
      #name of a element represents the synsetID and the value of a contains 
      #the value for such feature
      #
      #Args:
      #   synsetFeature: (character) it used to build the SynsetFeatureVector
      #
      #Returns:
      #   null
      #            
      
      private$synsetFeature <- synsetFeature
    },
    
    getSynsetsFeature = function() {
      #
      #Getter of synsetFeature
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of synsetFeature variable
      #
      return(private$synsetFeature)
    },
    
    getSize = function() {
      #
      #Gets the size (number of properties) of the current SynsetFeatureVector
      #
      #Args:
      #   null
      #
      #Returns:
      #   the size of the current SynsetFeatureVector
      #
      return(length(self$getSynsetsFeature()))
    },
    
    getFrequencyValue = function(synsetId) {
      #
      #Checks for the value stored for the synset synsetId
      #
      #Args:
      #   synsetId: (character) The target synset 
      #
      #Returns:
      #   The value asociated to synsetId, which represents the frequency of 
      #   appearance of the synsetId. If the synset is not found, -1 is returned.
      #      
      if ( synsetId %in% names(self$getSynsetsFeature())) {
        return(self$getSynsetsFeature()[[synsetId]])
      } else {
        return(-1)
      }
    }
  ),
  private = list(
    synsetFeature = list()
  )
)
