#
#Variables:
#
#propertyName: 
#
SynsetDictionary <- R6Class(
  
  "SynsetDictionary",
  
  public = list(
    
    initialize = function() {


    },
    
    add = function(synsetId) {
      private$synsetIdsHashSet <- list.append(private$synsetIdsHashSet, synsetId)
    },
    
    getSynsetIdsHashSet = function() {
      return(private$synsetIdsHashSet)
    },
    
    isIncluded = function() {
      return(synsetId %in% self$getSynsetIdsHashSet())
    }
    

  ),
  
  private = list(
    # 
    # The information storage for the dictionary. Only a Hashset of synsetsId
    # is requirede pair is the portion of the fixedText that matches the
    # 
    synsetIdsHashSet = list()
  )
)
