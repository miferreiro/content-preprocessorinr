DataWarc <- R6Class(
    classname = "DataWarc",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainDate = function(...){
            # private$date <-  tryCatch(read_warc_entry(self$getPath(),0)[[1]][['warc-date']],
            #                           warning = function(w) {
            #                               print("Date warc warning");
            #                               print("");
            #                           },
            #                           error = function(e) {
            #                               print(c("Date warc error",self$getPath()));
            #                               print("");
            #                           })
            private$date <- "fecha"
        },
        obtainSource = function(){
            pathExpand <- path.expand(private$path)
            fil <- file(pathExpand, "rb")
            if(!isSeekable(fil)){return("error")}
          
            seek(fil, where = 0,origin = "end")
            numCaracteres <-  seek(fil, where = 0,origin = "end")

            #i = 0;
            posRegistro = 0;
            while(numCaracteres - 2 >  posRegistro){ 
                
               # cat("\n")
                #cat("-- ",i,"--",posRegistro,"\n")
                #cat("\n")
                salida <- read_warc_entry(pathExpand,posRegistro )
                posRegistro <- salida[["warc_header"]][["longitud"]]

                tipo <- salida[["warc_header"]][["warc-type"]]
 
                if( equals(tipo,"response")  || equals(tipo,"resource")){
                    value = ""
                    
                    if(equals(tipo,"response") ){
                        value = salida[["headers"]][["content-type"]]
                        #cat(salida[["headers"]][["content-type"]]);
                       # cat("\n")
                        #comprueba que en la respuesta del servidor la entrada Content-Type: text/...
                        if ((grepl("text/plain", tolower(value)) )|| (grepl("text/html",tolower(value)))){
                            cat(salida[["headers"]][["content-type"]]);
                            private$source = rawToChar(salida[["content"]], multiple = FALSE);
                        }
                    }else{
                        
                        if(equals(tipo,"resource")){
                            value = salida[["headers"]][["content-type"]]
                            if ((grepl("text/plain", tolower(value)) )|| (grepl("text/html",tolower(value)))){
                                cat(salida[["headers"]][["content-type"]]);
                                private$source = rawToChar(salida[["content"]], multiple = FALSE);
                            }
                            
                        }
                    }
                    
                    
                }
                
            }
            closeAllConnections()
           
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