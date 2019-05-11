#Super class that handles the general functionalities of the management of
#the instances
#
#The tasks of the functions, that the Instance class has, are to handle
#the variables associated with an instance
#
#Variables:
#
#date: (character) The date on which the source was generated or sent
#source: (character) The text of the file without modifications
#path: (character) Identifier of the instance, in this case it will be the
#                       path of the file from which the properties are extracted
#data: (character) The text of the file with modifications
#properties: (list) Contains a list of properties extracted from the text
#                       that is being processed
#isValid: (logical) Indicates if the instance is valid or not
#flowPipes: (list) The list contains the pipes that the instance has passed through
#banPipes: (array) The list contains the pipes that can not be executed from that moment
Instance <- R6Class(
  
  "Instance",
  
  public = list(
    
    initialize = function(path) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of path.This variable
      #contains the path of the file to process.
      #In addition, the initial path property is initialized, just in case
      #the path of the file is changed and it is decided to save
      #the file in cache.
      #
      #Args:
      #   path: (character) Path of the file
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(path)) {
        stop("[Instance][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }
      
      private$path <- path
      
      self$addProperties(self$getPath(), "Initial_path")
      
    },
    
    obtainDate = function() {
      #
      #Abtract method to obtain the date
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
      stop("[Instance][obtainDate][Error]
              I'm an abstract interface method")
    },
    
    obtainSource = function() {
      #
      #Abtract method to obtain the source
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
      stop("[Instance][obtainSource][Error]
              I'm an abstract interface method")
    },
    
    getDate = function() {
      #
      #Getter of date variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of date variable
      #
      return(private$date)
    },
    
    getSource = function() {
      #
      #Getter of source variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of source variable
      #
      return(private$source)
    },
    
    getPath = function() {
      #
      #Getter of path variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of path variable
      #
      return(private$path)
    },
    
    getData = function() {
      #
      #Getter of data variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of data variable
      #
      return(private$data)
    },
    
    getProperties = function() {
      #
      #Getter of properties variable
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of properties variable
      #
      return(private$properties)
    },
    
    setSource = function(source) {
      #
      #Setter of source variable
      #
      #Args:
      #   source: (character) the new value of source variable
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(source)) {
        stop("[Instance][setSource][Error]
                Checking the type of the variable: source ",
                  class(source))
      }
      
      private$source <- source
      
      return()
    },
    
    setDate = function(date) {
      #
      #Setter of date variable
      #
      #Args:
      #   source: (character) the new value of date variable
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(date)) {
        stop("[Instance][setDate][Error]
                Checking the type of the variable: date ",
                  class(date))
      }
      
      private$date <- date
      
      return()
    },
    
    setProperties = function(properties) {
      #
      #Setter of properties variable
      #
      #Args:
      #   properties: (list) the new value of properties variable
      #
      #Returns:
      #   null
      #
      if (!"list" %in% class(properties)) {
        stop("[Instance][setProperties][Error]
                Checking the type of the variable: properties ",
                  class(properties))
      }
      
      private$properties <- properties
      
      return()
    },
    
    addProperties = function(propertyValue, propertyName) {
      #
      #Add a property to the list of properties
      #
      #Args:
      #   propertyValue: () the value of the new property
      #   propertyName: (character) the name of the new property
      #
      #Returns:
      #   null
      #
      
      if (!"character" %in% class(propertyName)) {
        stop("[Instance][addProperties][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      private$properties <- list.append(self$getProperties(), propertyValue)
      
      names(private$properties)[length(self$getProperties())] <- propertyName
      
      return()
    },
    
    getSpecificProperty = function(propertyName) {
      #
      #Obtains a specific property
      #
      #Args:
      #   propertyName: (character) the name of the property to obtain
      #
      #Returns:
      #   the value of property
      #
      if (!"character" %in% class(propertyName)) {
        stop("[Instance][getSpecificProperty][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      return(self$getProperties()[[propertyName]])
    },
    
    isSpecificProperty = function(propertyName) {
      #
      #Obtains if exists a specific property
      #
      #Args:
      #   propertyName: (character) the name of the property to check
      #
      #Returns:
      #   TRUE or FALSE depending on whether the property is on the list of properties
      #
      return(propertyName %in% self$getNamesOfProperties())
    },
    
    setSpecificProperty = function(propertyName, propertyValue) {
      #
      #Changes the value of the one property
      #
      #Args:
      #   propertyValue: () the new value of the property
      #   propertyName: (character) the name of the  property
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(propertyName)) {
        stop("[Instance][setSpecificProperty][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      private$properties[[nombrePropiedad]] <- propertyValue
      
      return()
    },
    
    getNamesOfProperties = function() {
      #
      #Getter of the names of properties
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of the names of properties
      #
      return(self$getProperties() %>>% names())
    },
    
    setData = function(data) {
      #
      #Setter of data variable
      #
      #Args:
      #   data: (character) the new value of data variable
      #
      #Returns:
      #   null
      #
      if (!"character" %in% class(data)) {
        stop("[Instance][setData][Error]
                Checking the type of the variable: data ",
                  class(data))
      }
      
      private$data <- data
      
      return()
    },
    
    isInstanceValid = function() {
      #
      #Obtains if the Instance is valid
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of isValid
      #
      return(private$isValid)
    },
    
    invalidate = function() {
      #
      #Sets the instance in the invalid state
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
      
      private$isValid <- FALSE
      
      return()
    },
    
    getFlowPipes = function() {
      #
      #Get the pipe flow list
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pipe ban list
      #
      return(private$flowPipes)
    },
    
    addFlowPipes = function(namePipe) {
      #
      #Added the name of the pipe to the list that keeps track of the flow of 
      #pipes that the instance has gone through
      #
      #Args:
      #   namePipe: (character) pipe name to be introduced into the flow
      #
      #Returns:
      #   null
      #      
      if (!"character" %in% class(namePipe)) {
        stop("[Instance][addFlowPipes][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }
      
      private$flowPipes <- list.append(private$flowPipes, namePipe)
      
      return()
    },
    
    getBanPipes = function() {
      #
      #Get the pipe ban array
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of pipe ban array
      #
      return(private$banPipes)
    },
    
    addBanPipes = function(namePipe) {
      #
      #Added the name of the pipe to the array that keeps track pipes that can 
      #not be run afeter
      #
      #Args:
      #   namePipe: (character) pipe name to be introduced into the ban array
      #
      #Returns:
      #   null
      #   
      if (!"character" %in% class(namePipe) & !is.null(namePipe)) {
        stop("[Instance][addBanPipes][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }
      
      if (!is.null(namePipe)) {
        private$banPipes <- c(private$banPipes, namePipe)
      }
      
      return()
    },
    
    checkCompatibility = function(namePipe, alwaysBefore) {
      #
      #Check compability between pipes.
      #
      #Args:
      #   namePipe: (character) name of the pipe to check the compatibility
      #   alwaysBefore: (list) pipes that the instance had to go through
      #Returns:
      #   TRUE/FALSE depends if the compability between pipes is correctly or not
      #      

      if (!"character" %in% class(namePipe)) {
        stop("[Instance][checkCompatibility][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }
      
      if (!"list" %in% class(alwaysBefore)) {
        stop("[Instance][checkCompatibility][Error]
                Checking the type of the variable: alwaysBefore ",
                  class(alwaysBefore))
      }
      
      for (depsB in alwaysBefore) {
        
        if (!depsB %in% self$getFlowPipes()) {
          return(FALSE)
        }
      }
      
      if (namePipe %in% self$getBanPipes()) {
        return(FALSE)
      }
      
      return(TRUE)
    }
  ),
  
  private = list(
    date = "",
    source = "",
    path = "",
    data = "",
    properties = list(),
    isValid = TRUE,
    flowPipes = list() ,
    banPipes = c()
  )
)