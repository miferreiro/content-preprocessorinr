#Class to handle warc files
#
#It is a class that inherits from the Instance class and implements
#the functions of extracting the text and the date of an warc-type file
#
#Variables:
#
ExtractorWarc <- R6Class(
  
  classname = "ExtractorWarc",
  
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
      #   path: (character) Path of the warc-type file
      #
      #Returns:
      #   null
      #
      
      if (!"character" %in% class(path)) {
        stop("[ExtractorWarc][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }
      path %>>%
        super$initialize()
    },
    
    obtainDate = function() {
      #
      #Function that obtain the date of the warc file
      #
      #Find the warcinfo type records in which the date appears and 
      # standardize it 
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #      

      xdfDate <- read_warc(super$getPath(), 
                           warc_types = c( "warcinfo"), 
                           include_payload = FALSE)

      for (i in 1:dim(xdfDate)[1]) {
        
          date <- xdfDate$date
          StandardizedDate <- as.POSIXct(date)
          formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
          format(StandardizedDate, formatDateGeneric) %>>%
            super$setDate()

      }
      
      return()
    },
    
    obtainSource = function() {
      #
      #Function that obtain the source of the warc file
      #
      #The list of records that contain information are obtained, 
      #which they are resource and response.
      #
      #Then they are traversed and the charset of that record is obtained.
      #If that charset matches the one obtained from guess_encoding, payload_content 
      #is used to get the contents of the record. If it does not match, the 
      #content is obtained, converting the content in bytes to string.
      #This is done because there are coding problems in cases that the charset 
      #that is detected is different from the one that is really
      #In addition it initializes the data with the initial source.
      # 
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
    
      rawData <- list()
      
      xdf <- read_warc(super$getPath(), 
                       warc_types = c( "response", "resource"), 
                       include_payload = TRUE)
      
      # cat("Longitud xdf: ", dim(xdf)[1],"\n")
      xdfHtmlPlain <- dplyr::filter(xdf, 
                                    grepl("(html|plain)", 
                                          http_protocol_content_type))

      numRecords <- dim(xdfHtmlPlain)[1]
      cat("[ExtractorWarc][obtainSource][Info] Numero de registros plain|html : ",
          numRecords,"\n")
      for (i in 1:numRecords) {

        if (grepl("response",xdfHtmlPlain$warc_type[i])) {

          cat("[ExtractorWarc][obtainSource][Info] response \n")
          
          charset <- toupper(str_match(pattern = "\\bcharset=\\s*\"?([^\\s;\"]*)", 
                                       xdfHtmlPlain$http_protocol_content_type[[i]])[2])

          if (!is.na(charset) && 
                guess_encoding(super$getPath())[[1]][[1]] == charset) {
            
            payload <-
              payload_content(
                url = xdfHtmlPlain$target_uri[i],
                ctype = xdfHtmlPlain$http_protocol_content_type[i],
                headers = xdfHtmlPlain$http_raw_headers[[i]],
                payload = xdfHtmlPlain$payload[[i]],
                enconding = charset,
                as = "text"
              )
  
             rawData <- list.append(rawData, payload)
             
          } else {
            
            rawData <- list.append(rawData,rawToChar(xdfHtmlPlain$payload[[1]]))
            
          }
            
        } else {
          
          if (grepl("resource", xdfHtmlPlain$warc_type[i])) {
            cat("[ExtractorWarc][obtainSource][Info] resource \n")
            charset <- toupper(str_match(pattern = "\\bcharset=\\s*\"?([^\\s;\"]*)",
                                         xdfHtmlPlain$http_protocol_content_type[[i]])[2])
            
            if (!is.na(charset) && 
                  guess_encoding(super$getPath())[[1]][[1]] == charset) {
              
              payload <-
                payload_content(
                  url = xdfHtmlPlain$target_uri[i],
                  ctype = xdfHtmlPlain$http_protocol_content_type[i],
                  headers = xdfHtmlPlain$http_raw_headers[[i]],
                  payload = xdfHtmlPlain$payload[[i]],
                  enconding = charset,
                  as = "text"
                )
              
              rawData <- list.append(rawData, payload)
              
            } else {
              
              rawData <- list.append(rawData, rawToChar(xdfHtmlPlain$payload[[1]]))  
              
            }
          }
        }
      }
      # In some cases the library returns an array of strings. 
      # Then everything is converted to a single string
      rawData <- paste(rawData, collapse = " ")
      
      rawData %>>%
        super$setSource()
      
      super$getSource() %>>%
        super$setData()
      
      return()
    }#End of the function obtainSource
  )
)