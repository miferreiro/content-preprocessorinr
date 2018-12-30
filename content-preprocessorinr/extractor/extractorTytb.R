#Class to handle tytb files
#
#It is a class that inherits from the ExtractorSource class and implements 
#the functions of extracting the text and the date of an Tytb-type file
#
#Variables:
#
ExtractorTytb <- R6Class(
    
  classname = "ExtractorTytb",
    
  inherit = ExtractorSource,
    
  public = list(
        
    initialize = function(path) {
      #
      #Class constructor 
      #
      #This constructor calls the constructor of the superclass to which 
      #it passes the path of the file
      #
      #Args: 
      #   path: (character) Path of the tytb-type file
      #
      #Returns: 
      #   null
      #      
      path %>>%
        super$initialize()
      
    },
    
    obtainDate = function(){
      #
      #Function that obtain the date of the tytb file
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #  
      private$date <- ""
      
      return()
    },
    
    obtainSource = function() {
      #
      #Function that obtain the source of the tytb file
      #
      #Reads the file indicated in the path and then transforms it to utf8.
      #In addition it initializes the data with the initial source.
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #       
      self$getPath() %>>%
        readLines() %>>%
          enc2utf8()  %>>%
            super$setSource()
            
      self$setData(private$source)
      
      return()
    }
  )
)