#
#Variables:
#
#propertyName: 
#
SynsetFeatureVector <- R6Class(
  
  "SynsetFeatureVector",
  
  public = list(
    
    initialize = function(synsetFeature) {
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
      return(length(self$getSynsetsFeature()))
    },
    
    getFrequencyValue = function(synsetId) {
      
      if ( synsetId %in% names(self$getSynsetsFeature())) {
        
        return(self$getSynsetsFeature()[[synsetId]])
      } else {
        
        return(-1)
        
      }
      
    }
  ),
  
  private = list(
    # 
    # A map of synsets together with its values
    # 
    synsetFeature = list()
  )
)
