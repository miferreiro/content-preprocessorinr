#Class to 
#
#
#Variables:
#

ResourceHandle <- R6Class(
  
  "ResourceHandle",
  
  public = list(
    
    initialize = function() {
      
    },
    
    isLoadResource = function(pathResource) {
      
      #
      #
      #Args: 
      #   
      #
      #Returns: 
      #   
      #           
      if (!"character" %in% class(pathResource)) {
        
        stop("[ResourceHandle][isLoadResource][Error] 
             Checking the type of the variable: pathResource ", 
             class(pathResource));
      }
      
      if (pathResource %in% self$getNamesResources()) {
        
        return(self$getResources()[[pathResource]])
        
      } else {
        
        if (file.exists(pathResource)) {
          
          jsonData <- fromJSON(file = pathResource)
          self$setResources(list.append(self$getResources(), jsonData))
          names(private$resources)[length(self$getResources())] <- pathResource
          
          return(self$getResources()[[pathResource]])
          
        } else {
          return(NULL)
        }
      }
      return(listResource)
    },
    
    getResources = function() {
      return(private$resources)
    },
    
    setResources = function(resources) {
      private$resources <- resources
    },
    getNamesResources = function() {
      return(names(self$getResources()))
    }
  ),
  
  private = list(
    resources = list()
  )
)