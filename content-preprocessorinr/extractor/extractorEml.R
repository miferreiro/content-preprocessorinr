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
    
    initialize = function(path, pathKeys = "content-preprocessorinr/config/configurations.ini") {
      #
      #Class constructor
      #
      #This constructor calls the constructor of the superclass to which
      #it passes the path of the file. In addition, obtain the configuration to read the eml files
      #
      #Args:
      #   path: (character) Path of the eml-type file
      #
      #Returns:
      #   null
      #
      path %>>%
        super$initialize()
      
      read.ini(pathKeys)$eml$PartSelectedOnMPAlternative %>>%
        self$setPartSelectedOnMPAlternative()
    },
    
    obtainDate = function() {
      #
      #Function that obtain the date of the eml file
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
        
        read_emails(super$getPath(),self$getPartSelectedOnMPAlternative())@date,
        
        warning = function(w) {
          warning(paste("Date eml warning ", super$getPath()," ", paste(w), "\n"))
          print("")
        },
        
        error = function(e) {
          warning(paste("Date eml error ", super$getPath(), " ", paste(e), "\n"))
          print("")
        }
      )
       tryCatch({
         formatDateEml <- "%a, %d %b %Y %H:%M:%S %z"
         StandardizedDate <- as.POSIXct(dateEml[[1]], format = formatDateEml)
         formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
         format(StandardizedDate, formatDateGeneric) %>>%
            super$setDate()
         },
         error = function(e) {
           warning(paste("Date eml error in standardized proccess", super$getPath(), " ", paste(e), "\n"))
           print("")
         }
       )
       
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
        
        paste(read_emails(super$getPath(), self$getPartSelectedOnMPAlternative())@message,collapse=" "),
        
        warning = function(w) {
          warning(paste("Source eml warning ", super$getPath(), " ", paste(w),"\n"))
          print("")
        },
        
        error = function(e) {
          warning(paste("Source eml error ", super$getPath()," ", paste(e), "\n"))
          print("")
        }
      )
      
      super$getSource() %>>%
        super$setData()
      
      return()
    },
    getPartSelectedOnMPAlternative = function() {
      #
      #Getter of of PartSelectedOnMPAlternative
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of PartSelectedOnMPAlternative
      #      
      return(private$PartSelectedOnMPAlternative)
    },
    setPartSelectedOnMPAlternative = function(PartSelectedOnMPAlternative) {
      #
      #Setter of PartSelectedOnMPAlternative variable
      #
      #Args:
      #   PartSelectedOnMPAlternative: (character) the new value of PartSelectedOnMPAlternative variable
      #
      #Returns:
      #   null
      #      
      private$PartSelectedOnMPAlternative <- PartSelectedOnMPAlternative
    }   
    
  ),

  private = list(
    PartSelectedOnMPAlternative = ""
  )
)
