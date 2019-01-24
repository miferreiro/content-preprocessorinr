#Class to handle eml files
#
#It is a class that inherits from the Instance class and implements 
#the functions of extracting the text and the date of an eml-type file
#
#Variables:
#
ExtractorEml <- R6Class(
    
  classname = "ExtractorEml",
    
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
      #   path: (character) Path of the eml-type file
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
      #Call the function read_emails and obtain the date of the file indicated 
      #in the path and then transforms it into the generic date format that is
      #"%a %b %d %H:%M:%S %Z %Y"
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #              
      dateEml <- tryCatch(
                           read_emails(self$getPath())@date,
                      
                           warning = function(w) {
                             cat("Date eml warning ",self$getPath(),"\n")
                             print("")
                           },
                           
                           error = function(e) {
                             cat("Date eml error ",self$getPath(),"\n")
                             print("")
                           }
                         )
            
      formatDateEml <- "%a, %d %b %Y %H:%M:%S %z"
      StandardizedDate <- as.POSIXct(dateEml,format = formatDateEml)
      formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
      private$date <- format(StandardizedDate,formatDateGeneric)
        
      return()
    },     
        
    obtainSource = function() {
      #
      #Function that obtain the source of the eml file
      #
      #Call the function read_emails and obtain the source of the file indicated 
      #in the path and then transforms it to utf8.
      #In addition it initializes the data with the initial source.
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #              
      private$source <- tryCatch(
                                  enc2utf8(read_emails(self$getPath())@message ),
                                   
                                  warning = function(w) {
                                    cat("Source eml warning ", 
                                         self$getPath(), "\n")
                                    print("")
                                  },
                                   
                                  error = function(e) {
                                    cat("Source eml error ", 
                                         self$getPath(), "\n")
                                    print("")
                                  }
                                )
            
      self$setData(private$source)
      
      return()
    }
  )
)
