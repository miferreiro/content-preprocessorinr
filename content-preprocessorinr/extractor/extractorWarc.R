ExtractorWarc <- R6Class(
    classname = "ExtractorWarc",
    inherit = ExtractorSource,
    public = list(
        initialize = function(path) {
            super$initialize(path)
            # super$addProperties(generalFun$getTarget(super$getPath()),"target")
            # super$obtainSourceDate()
            # ifelse(!(validUTF8(super$getSource())),
            #        {  
            #            mensaje <- c( "el archivo " , super$getPath() , " no es utf8")
            #            warning(mensaje)
            #        }
            #        ,"")
        },
        obtainDate = function(){
            tryCatch({
            
                dateObtained = ""
                pathExpand <- path.expand(private$path)
                fil <- file(pathExpand, "rb")
                ifelse((!isSeekable(fil)),{return("error")},"")
                
                seek(fil, where = 0,origin = "end")
                numCaracteres <-  seek(fil, where = 0,origin = "end")
                #i = 0;
                posRegistro = 0
                while (numCaracteres - 2 >  posRegistro) { 
                    
                    salida <- read_warc_entry(pathExpand,posRegistro )
                    posRegistro <- salida[["warc_header"]][["longitud"]]
                    tipo <- salida[["warc_header"]][["warc-type"]]
                    #print(tipo);
                    ifelse((equals(tipo,"warcinfo")),{
                        dateObtained = salida[["warc_header"]][["WARC-Date"]];break;
                    },"")
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
                if ( equals(tipo,"response")  || equals(tipo,"resource")){
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
                        ifelse(((grepl("text/plain", tolower(value)) ) || 
                                    (grepl("text/html",tolower(value)))),{
                          #  print(salida[["headers"]][["content-type"]]); #Ejemplo: text/html; charset=UTF-8
                            rawData <- rawToChar(salida[["content"]], multiple = FALSE);
                            break;
                        },"")
                    }else{
                        
                        ifelse((equals(tipo,"resource")),{
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
           
        }
    )
)