#Class to handle ttwt files
#
#It is a class that inherits from the Instance class and implements
#the functions of extracting the text and the date of an ttwt-type file
#
#Variables:
#
ExtractorTtwt <- R6Class(
  
  classname = "ExtractorTtwt",
  
  inherit = Instance,
  
  public = list(
    
    initialize = function(path) {
      #
      #Class constructor
      #
      #This constructor calls the constructor of the superclass to which
      #it passes the path of the file
      #
      #Args:
      #   path: (character) Path of the ttwt-type file
      #
      #Returns:
      #   null
      #
      path %>>%
        super$initialize()
      
    },
    
    obtainDate = function() {
      #
      #Function that obtain the date of the ttwd file
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
      "" %>>%
        super$setDate()
      
      return()
    },
    
    obtainSource = function() {
      #
      #Function that obtain the source of the ttwd file
      #
      #Reads the file indicated in the path and then transforms it to utf8.
      #In addition it initializes the data with the initial source.
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
      super$getPath() %>>%
        read_file() %>>%
          iconv(to = "utf-8") %>>%
            super$setSource()
      
      super$getSource() %>>%
        super$setData()
      
      return()
    }
  )
)