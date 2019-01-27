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
      tryCatch({
        
        dateWarc <- ""
        
        pathExpand <- path.expand(super$getPath())
        
        fil <- file(pathExpand, "rb")
        
        if (!isSeekable(fil)) {
          return("error")
        }
        
        # seek(fil, where = 0, origin = "end")
        #Se obtiene el numero de caracteres del fichero
        numCaracteres <-  seek(fil, where = 0, origin = "end")
        
        posNextRecord <- 0
        # Se recorren todos los registros hasta llegar al final del fichero
        while (numCaracteres - 2 >  posNextRecord) {
          #Se obtiene el registro
          salida <- read_warc_entry(pathExpand, posNextRecord)
          #Se obtiene la posición del siguiente registro
          posNextRecord <- salida[["warc_header"]][["longitud"]]
          #Se obtiene el tipo del registro
          warcRecordType <- salida[["warc_header"]][["warc-type"]]
          
          #Buscamos si el registro es de tipo warcinfo y obtenemos la fecha
          if (warcRecordType %in% "warcinfo") {
            dateWarc <- salida[["warc_header"]][["WARC-Date"]]
            break
          }
        }#Fin while
        
        StandardizedDate <- as.POSIXct(dateWarc)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        format(StandardizedDate, formatDateGeneric) %>>%
          super$getDate()
        
        closeAllConnections()
      },
      
      warning = function(w) {
        
        closeAllConnections()
        warning(paste("Date warc warning"))
        print("")
      },
      
      error = function(e) {
        closeAllConnections()
        warning(paste("Date warc error"))
        print("")
      })
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
      tryCatch(
        {
        rawData <- ""
          
        pathExpand <- path.expand(private$path)
        
        fil <- file(pathExpand, "rb")
        
        if (!isSeekable(fil)) {
          return("error")
        }
        
        # seek(fil, where = 0, origin = "end")
        #Se obtiene el numero de caracteres del fichero
        numCaracteres <-  seek(fil, where = 0, origin = "end")
        
        posNextRecord <- 0
        # Se recorren todos los registros hasta llegar al final del fichero
        while (numCaracteres - 2 >  posNextRecord) {
          #Se obtiene el registro
          salida <- read_warc_entry(pathExpand, posNextRecord)
          #Se obtiene la posición del siguiente registro
          posNextRecord <- salida[["warc_header"]][["longitud"]]
          #Se obtiene el tipo del registro
          warcRecordType <- salida[["warc_header"]][["warc-type"]]
          
          # Solo comprobamos los registros si son del tipo responde o resource
          if (warcRecordType %in% "response"  ||
                warcRecordType %in% "resource") {
            #Donde se almacenara el content-type
            value <- "" 
            #rawData <- salida[["warc_header"]];
            #print(rawData)
            
            if (equals(warcRecordType, "response")) {
              value <- salida[["headers"]][["content-type"]]
              #cat(salida[["headers"]][["content-type"]]);
              # cat("\n")
              #comprueba que en la respuesta del servidor la entrada Content-Type: text/...
              value <- salida[["headers"]][["content-type"]]
              
              if (grepl("text/plain", tolower(value)) ||
                      grepl("text/html", tolower(value))) {
                  #print(salida[["headers"]][["content-type"]]); #Ejemplo: text/html; charset=UTF-8
                  rawData <- rawToChar(salida[["content"]], multiple = FALSE)
                  
                  break
                  
              }
              
            } else{
              if (warcRecordType %in% "resource") {
                value <- salida[["headers"]][["content-type"]]
                
              }
              
              if (grepl("text/plain", tolower(value)) ||
                    grepl("text/html", tolower(value))) {
                  # print(salida[["headers"]][["content-type"]]);#Ejemplo: text/html; charset=UTF-8
                  rawData <- rawToChar(salida[["content"]], multiple = FALSE)
                  
                  break
                  
              }#Fin if
            }#Fin else
          }#Fin if
        }#Fin while
        
        
        # cat("rawData " )
        # print(substr(rawData,0,300));
        # cat("\n");
        iconv(rawData, to = "utf-8") %>>%
          super$setSource()
        
        super$getSource() %>>%
          super$setData()
        
        closeAllConnections()
        
      },
      warning = function(w) {
        closeAllConnections()
        warning("Date warc warning")
        
        print("")
        
      },
      error = function(e) {
        closeAllConnections()
        warning("Date warc error")
        
        print("")
        
      })
    }#End of the function obtainSource
  )
)