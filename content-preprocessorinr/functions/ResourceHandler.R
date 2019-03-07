#Class to manage different types of resources
#
#Variables:
#resources: (list) variable that stores the lists of the different types of resources
#
ResourceHandler <- R6Class(
  
  "ResourceHandler",
  
  public = list(
    
    initialize = function() {
      
    },
    
    isLoadResource = function(pathResource) {
      
      #From the resource path, it is checked if they have already been loaded. 
      #In this case, the list of the requested resource is returned. Otherwise, 
      #the resource variable is added to the list of resources, and the resource 
      #list is returned.
      #In the event that the resource file does not exist, NULL is returned.
      # 
      #Args: 
      #pathResource: (character) resource file path   
      #
      #Returns: 
      #   
      #           
      if (!"character" %in% class(pathResource)) {
        stop("[ResourceHandler][isLoadResource][Error] 
                Checking the type of the variable: pathResource ", 
                  class(pathResource));
      }
      
      if (pathResource %in% self$getNamesResources()) {
        
        return(self$getResources()[[pathResource]])
        
      } else {
        
        if (file.exists(pathResource)) {
          
          jsonData <- rjson::fromJSON(file = pathResource)
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
      #
      #Getter of resources variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of resources variable
      #
      return(private$resources)
    },
    
    setResources = function(resources) {
      #
      #Setter of resources variable
      #
      #Args:
      #   resources: (character) the new value of source resources
      #
      #Returns:
      #   null
      #
      private$resources <- resources
      
      return()
    },
    getNamesResources = function() {
      #
      #Getter of names of resources
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of names of resources
      #
      return(names(self$getResources()))
    }
  ),
  
  private = list(
    resources = list()
  )
)