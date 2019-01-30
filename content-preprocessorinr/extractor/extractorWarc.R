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
      path %>>%
        super$initialize()
    },
    
    obtainDate = function() {
      #
      #Function that obtain the date of the warc file
      #
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #      
      path <- super$getPath()
      
      xdfDate <- read_warc(path, warc_types = c( "warcinfo"), include_payload = FALSE)
      cat("Hay ",dim(xdfDate)[1]," registros de warcinfo\n")
      for (i in 1:dim(xdfDate)[1]) {
        if (grepl("warcinfo",xdfDate$warc_type[i])) {
          date <- xdfDate$date
          StandardizedDate <- as.POSIXct(date)
          formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
          format(StandardizedDate, formatDateGeneric) %>>%
            super$setDate()

        }
      }
    },
    
    obtainSource = function() {
      #
      #Function that obtain the source of the warc file
      #
      #
      #
      #In addition it initializes the data with the initial source.
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
    
      path <- super$getPath()
      rawData <- list()
      xdf <- read_warc(path, warc_types = c( "response", "resource"), include_payload = TRUE)
      cat("Longitud xdf: ", dim(xdf)[1],"\n")
      xdfHtmlPlain <- dplyr::filter(xdf, grepl("(html|plain)", http_protocol_content_type))

      # View(xdfHtmlPlain)

      numRecords <- dim(xdfHtmlPlain)[1]
      cat("numero de registros plain|html : ", numRecords,"\n")

      for (i in 1:numRecords) {

        if (grepl("response",xdfHtmlPlain$warc_type[i])) {
          print("response")

          # charset <- grep("(?:(charset=))([A-Za-z0-9-]*)", 
          #                 xdfHtmlPlain$http_protocol_content_type[[i]], value = T)
          # print(charset)
          # charset <- ""
          # b <- payload_content(url = xdfHtmlPlain$target_uri[i], ctype = xdfHtmlPlain$http_protocol_content_type[i],
          #                      headers = xdfHtmlPlain$http_raw_headers[[i]], payload = xdfHtmlPlain$payload[[i]],enconding = charset, as = "parsed")
          # 
          rawData <- list.append(rawData,rawToChar(xdfHtmlPlain$payload[[1]]))
         
         

          
        } else {
          if (grepl("resource", xdfHtmlPlain$warc_type[i])) {
            print("resource")
            print(xdfHtmlPlain$warc_type[i])
            
            rawData <- list.append(rawData,rawToChar(xdfHtmlPlain$payload[[1]]))
            
          }
        }
      }
      
      rawData <- paste(rawData,collapse = " ")
      
      iconv(rawData, to = "utf-8") %>>%
        super$setSource()
      
      super$getSource() %>>%
        super$setData()
      
    }#End of the function obtainSource
  )
)