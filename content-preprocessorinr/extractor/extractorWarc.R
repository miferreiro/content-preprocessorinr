#Class to handle warc files
#
#It is a class that inherits from the ExtractorSource class and implements 
#the functions of extracting the text and the date of an warc-type file
#
#Variables:
#
ExtractorWarc <- R6Class(
    
  classname = "ExtractorWarc",
    
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
      #   path: (character) Path of the warc-type file
      #
      #Returns: 
      #   null
      #      
      path %>>% 
        super$initialize()
          
    },
        
    obtainDate = function(){
      tryCatch(
        {
        
          dateWarc <- ""
            
          pathExpand <- path.expand(private$path)
          
          fil <- file(pathExpand, "rb")
          
          ifelse((!isSeekable(fil)),{return("error")},"")
          
          seek(fil, where = 0,origin = "end")
          
          numCaracteres <-  seek(fil, where = 0,origin = "end")
          
          posNextRecord <- 0
          
          while (numCaracteres - 2 >  posNextRecord) { 
              
            salida <- read_warc_entry(pathExpand,posNextRecord )
            
            posNextRecord <- salida[["warc_header"]][["longitud"]]
            
            warcRecordType <- salida[["warc_header"]][["warc-type"]]
            
            ifelse((equals(warcRecordType,"warcinfo")),{
              dateWarc = salida[["warc_header"]][["WARC-Date"]];break;
            },"")
          }#Fin while
          
          StandardizedDate <- as.POSIXct(dateWarc)
          formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
          private$date <- format(StandardizedDate,formatDateGeneric)

          closeAllConnections()
        },
        
        warning = function(w) {
            closeAllConnections()
            print("Date warc warning")
            print("");
        },
        
        error = function(e) {
            closeAllConnections()
            print(c("Date warc error"))
            print("")
        }
      )
    },
        
    obtainSource = function(){
        
      tryCatch(
        {
        
          pathExpand <- path.expand(private$path)
          fil <- file(pathExpand, "rb")
          if (!isSeekable(fil)) {
            return("error")
          }
      
          seek(fil, where = 0,origin = "end")
          numCaracteres <-  seek(fil, where = 0,origin = "end")
        
          posNextRecord <- 0
          rawData <- ""
          while (numCaracteres - 2 >  posNextRecord) { 
          
            salida <- read_warc_entry(pathExpand,posNextRecord )
            
            posNextRecord <- salida[["warc_header"]][["longitud"]]
            
            warcRecordType <- salida[["warc_header"]][["warc-type"]]
            # print(tipo)
            if ( equals(warcRecordType,"response")  || equals(warcRecordType,"resource")){
                # cat(tipo)
                # cat("\n")
              value <- "" #Donde se almacenara el content-type
                #rawData <- salida[["warc_header"]];
                #print(rawData)
              if (equals(warcRecordType,"response")) {
                value <- salida[["headers"]][["content-type"]]
                  #cat(salida[["headers"]][["content-type"]]);
                 # cat("\n")
                  #comprueba que en la respuesta del servidor la entrada Content-Type: text/...
                value <- salida[["headers"]][["content-type"]];
                ifelse(((grepl("text/plain", tolower(value)) ) || 
                              (grepl("text/html",tolower(value)))),{
                    #  print(salida[["headers"]][["content-type"]]); #Ejemplo: text/html; charset=UTF-8
                      rawData <- rawToChar(salida[["content"]], multiple = FALSE);
                      break;
                  },"")
                
              }else{
                    
                ifelse((equals(warcRecordType,"resource")),{
                    value <- salida[["headers"]][["content-type"]];
                },"")
                
                ifelse(((grepl("text/plain", tolower(value)) ) || 
                            (grepl("text/html",tolower(value)))),{
                   # print(salida[["headers"]][["content-type"]]);#Ejemplo: text/html; charset=UTF-8 
                    rawData <- rawToChar(salida[["content"]], multiple = FALSE);
                    break;
                },"")#Fin if

              }#Fin else
            }#Fin if
          }#Fin while
            
        
            # cat("rawData " )
            # print(substr(rawData,0,300));
            # cat("\n");
            private$source <- enc2utf8(rawData);
            self$setData(private$source)
            closeAllConnections()
            
         }
        ,warning = function(w) {
            closeAllConnections()
            print("Date warc warning");
            print("");
        },
        error = function(e) {
            closeAllConnections()
            print(c("Date warc error"));
            print("");
        }
      )
    }#End of the function obtainSource
  )
)