DataWarc <- R6Class(
    classname = "DataWarc",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainDate = function(...){
            tryCatch({
            
            dateObtained = ""
            pathExpand <- path.expand(private$path)
            fil <- file(pathExpand, "rb")
            if(!isSeekable(fil)){return("error")}
            
            seek(fil, where = 0,origin = "end")
            numCaracteres <-  seek(fil, where = 0,origin = "end")
            #i = 0;
            posRegistro = 0
            rawData = ""
            while(numCaracteres - 2 >  posRegistro){ 
                
                salida <- read_warc_entry(pathExpand,posRegistro )
                posRegistro <- salida[["warc_header"]][["longitud"]]
                tipo <- salida[["warc_header"]][["warc-type"]]
                #print(tipo);
                if( equals(tipo,"warcinfo")){
                    dateObtained = salida[["warc_header"]][["WARC-Date"]];
                  
                }
            }#Fin while
            dateObtained <- as.POSIXct(dateObtained)
            formato <- "%a %b %d %H:%M:%S %Z %Y"
            private$date <- format(dateObtained,formato)

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
        },
        obtainSource = function(){
            
            tryCatch({
            
            pathExpand <- path.expand(private$path)
            fil <- file(pathExpand, "rb")
            if(!isSeekable(fil)){return("error")}
          
            seek(fil, where = 0,origin = "end")
            numCaracteres <-  seek(fil, where = 0,origin = "end")
            #i = 0;
            posRegistro = 0;
            rawData = ""
            while(numCaracteres - 2 >  posRegistro){ 
                
                #cat("\n")
                #cat("-- ",i,"--",posRegistro,"\n")
                #cat("\n")
                salida <- read_warc_entry(pathExpand,posRegistro )
                posRegistro <- salida[["warc_header"]][["longitud"]]
                tipo <- salida[["warc_header"]][["warc-type"]]
                # print(tipo);
                if( equals(tipo,"response")  || equals(tipo,"resource")){
                    # cat(tipo);
                    # cat("\n");
                   value <- "";#Donde se almacenara el content-type
                   #rawData <- salida[["warc_header"]];
                    #print(rawData)
                    if(equals(tipo,"response") ){
                        value <- salida[["headers"]][["content-type"]]
                        #cat(salida[["headers"]][["content-type"]]);
                       # cat("\n")
                        #comprueba que en la respuesta del servidor la entrada Content-Type: text/...
                        value <- salida[["headers"]][["content-type"]];
                        if ((grepl("text/plain", tolower(value)) )|| (grepl("text/html",tolower(value)))){
                          #  print(salida[["headers"]][["content-type"]]); #Ejemplo: text/html; charset=UTF-8
                            rawData <- rawToChar(salida[["content"]], multiple = FALSE);
                        }
                    }else{
                        
                        if(equals(tipo,"resource")){
                            value <- salida[["headers"]][["content-type"]];
                        }
                        
                        if ((grepl("text/plain", tolower(value)) )|| (grepl("text/html",tolower(value)))){
                           # print(salida[["headers"]][["content-type"]]);#Ejemplo: text/html; charset=UTF-8 
                            rawData <- rawToChar(salida[["content"]], multiple = FALSE);
                        }#Fin if

                        }#Fin else
                    }#Fin if
                }#Fin while
                
            
            # cat("rawData " )
            # print(substr(rawData,0,300));
            # cat("\n");
            private$source <- rawData;
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
           
        },
        getDate = function(){
            return(private$date)
        },
        getPath = function(){
            return(private$path);
        },
        getSource = function(){
            return(private$source)
        },
        getData = function(){
            return(private$data)
        },
        getProperties = function(){
            return(private$properties)
        },
        addProperties = function(valorPropiedad,nombrePropiedad){
            private$properties <-  list.append(private$properties,valorPropiedad)
            names(private$properties)[length(self$getProperties())] <- nombrePropiedad
        },
        getSpecificProperties = function(nombrePropiedad){
            return(private$properties[[nombrePropiedad]])
        },
        setSpecificProperties = function(nombrePropiedad,valorPropiedad){
            private$properties[[nombrePropiedad]] <- valorPropiedad        
        },
        setData = function(data){
            private$data = data
        }
    )
)