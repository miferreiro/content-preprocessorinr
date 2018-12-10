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
            if(file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosDate/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = ""))){
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosDate/_" , self$getSpecificProperties("target") , "_/" , self$id , ".twtid",sep = "")
                
                private$date <- readLines(private$path)
                
            }else{  
                get_env(conexiones)$comprobacionDePeticionesTwitter();
                
                twitteR:::check_id(as.character(self$getId()))
                date <- tryCatch(showStatus(as.character(self$getId()))$getCreated(),
                                           warning = function(w) {
                                               cat("Date twtid warning: ",paste(w));
                                               print("");
                                           },
                                           error = function(e) {
                                               cat("Date twtid error",self$getId()," ",paste(e));
                                               print("");
                                           })
                get_env(conexiones)$incrementContadorDePeticionesTwitter();
            
                 formato1 = "%Y-%m-%d %H:%M:%S %Z";
                    date <- as.POSIXct(date,format = formato1)
                    formato2 <- "%a %b %d %H:%M:%S %Z %Y"
                    private$date <- format(date,formato2)
                    
                    cat(as.character(private$date),
                        file = paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosDate/_",
                                     self$getSpecificProperties("target"),"_/",
                                     self$id ,".twtid",sep = ""),sep = "\n"
                    )
             
            }
        },
        obtainId = function(){
            self$id <- readLines(self$getPath(),warn = FALSE, n = 1)
        },
        obtainSource = function(){

                if(file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = ""))){
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_" , self$getSpecificProperties("target") , "_/" , self$id , ".twtid",sep = "")
                
                private$source <- enc2utf8(readLines(private$path))

            }else{
 
                  get_env(conexiones)$comprobacionDePeticionesTwitter();
                  
                  twitteR:::check_id(as.character(self$getId()))
                  private$source <- tryCatch(enc2utf8(showStatus(as.character(self$getId()))$getText()),
                                             warning = function(w) {
                                                 cat("Source twtid warning: ",paste(w));
                                                 print("");
                                             },
                                             error = function(e) {
                                                 cat("Source twtid error",self$getId()," ",paste(e))
                                                 print("");
                                             })
                  
                  get_env(conexiones)$incrementContadorDePeticionesTwitter();

                  cat(private$source,
                              file = paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_",
                                           self$getSpecificProperties("target"),"_/",
                                           self$id ,".twtid",sep = ""),sep = "\n"
                  )
            }
        }
    )
)