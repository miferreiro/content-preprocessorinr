#Class to handle sms files
#
#It is a class that inherits from the Instance class and implements 
#the functions of extracting the text and the date of an sms-type file
#
#Variables:
#
ExtractorSms <- R6Class(
    
  classname = "ExtractorSms",
    
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
      #   path: (character) Path of the sms-type file
      #
      #Returns: 
      #   null
      #      
      path %>>% 
        super$initialize()
      
    },
    
    obtainDate = function() {
      #
      #Function that obtain the date of the sms file
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
      #Function that obtain the source of the sms file
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