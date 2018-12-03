DataTwtid <- R6Class(
    classname = "DataTwtid",
    inherit = DataSource,
    public = list(
        
        initialize = function(path) {
            
            private$path <- path
            self$obtainId()
            #Se comprueba si se ha conetactdo con twitter.
            #En el caso de que no se haya conectado, se conecta.
            #Singleton
            get_env(conexiones)$startConectionWithTwitter()
            
        },
        id = "",
        obtainDate = function(){
           # print(file.exists(paste("content-preprocessor/cache/hsspam14/tweetsLeidosDate/_",self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = "")))
            if(file.exists(paste("content-preprocessor/cache/hsspam14/tweetsLeidosDate/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = ""))){
                
                
                private$path <- paste("content-preprocessor/cache/hsspam14/tweetsLeidosDate/_" , self$getSpecificProperties("target") , "_/" , self$id , ".twtid",sep = "")
                
                
                private$date <- readLines(paste("content-preprocessor/cache/hsspam14/tweetsLeidosDate/_",
                                                  self$getSpecificProperties("target"),"_/",
                                                  self$id ,".twtid",sep = ""))
                
            }else{  
                get_env(conexiones)$comprobacionDePeticionesTwitter();
                
                
                twitteR:::check_id(as.character(self$getId()))
                date <- tryCatch(showStatus(as.character(self$getId()))$getCreated(),
                                           warning = function(w) {
                                               print("Date twtid warning");
                                               print("");
                                           },
                                           error = function(e) {
                                               print(c("Date twtid error",self$getId()));
                                               print("");
                                           })
                get_env(conexiones)$incrementContadorDePeticionesTwitter();
        
                date <- as.POSIXct(date)
                formato <- "%a %b %d %H:%M:%S %Z %Y"
                private$date <- format(date,formato)
                
                cat(as.character(private$date),
                    file = paste("content-preprocessor/cache/hsspam14/tweetsLeidosDate/_",
                                 self$getSpecificProperties("target"),"_/",
                                 self$id ,".twtid",sep = ""),sep = "\n"
                )
                
            }
        },
        obtainId = function(){
            self$id <- readLines(self$getPath(),warn=FALSE, n = 1)
        },
        obtainSource = function(){
            # print(file.exists(paste("content-preprocessor/cache/hsspam14/tweetsLeidosSource/_",self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = "")))
            if(file.exists(paste("content-preprocessor/cache/hsspam14/tweetsLeidosSource/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = ""))){
                
                
                private$path <- paste("content-preprocessor/cache/hsspam14/tweetsLeidosSource/_" , self$getSpecificProperties("target") , "_/" , self$id , ".twtid",sep = "")
                
                
                private$source <- readLines(paste("content-preprocessor/cache/hsspam14/tweetsLeidosSource/_",
                                self$getSpecificProperties("target"),"_/",
                                self$id ,".twtid",sep = ""))

            }else{
 
                  get_env(conexiones)$comprobacionDePeticionesTwitter();
                  
                  twitteR:::check_id(as.character(self$getId()))
                  private$source <- tryCatch(showStatus(as.character(self$getId()))$getText(),
                                             warning = function(w) {
                                                 print("Source twtid warning");
                                                 print("");
                                             },
                                             error = function(e) {
                                                 print(c("Source twtid error",self$getId()));
                                                 print("");
                                             })
                  
                  get_env(conexiones)$incrementContadorDePeticionesTwitter();

                  cat(private$source,
                              file = paste("content-preprocessor/cache/hsspam14/tweetsLeidosSource/_",
                                           self$getSpecificProperties("target"),"_/",
                                           self$id ,".twtid",sep = ""),sep = "\n"
                  )
            }
           
          
        },
        getDate = function(){
            return(private$date)
        },
        getId = function(){
            return(self$id);
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