ExtractorTwtid <- R6Class(
    classname = "ExtractorTwtid",
    inherit = ExtractorSource,
    public = list(
        initialize = function(path) {
            
            private$path <- path
            self$obtainId()
            #Se comprueba si se ha conetactdo con twitter.
            #En el caso de que no se haya conectado, se conecta.
            #Singleton
            get_env(connections)$startConectionWithTwitter()
            
        },
        id = "",
        status = NULL,
        obtainDate = function(){
            if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosDate/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = ""))){
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosDate/_" , self$getSpecificProperties("target") , "_/" , self$id , ".twtid",sep = "")
                private$date <- readLines(private$path)
                ifelse((private$date == ""),{
                    mensaje <- c( "el archivo " , x$getPath() , " tiene la fecha vacia")
                    warning(mensaje)
                },"")
            }else{  
                ifelse((is.null(self$status)),{
                    get_env(connections)$checkRequestToTwitter();
                    
                    twitteR:::check_id(as.character(self$getId()))
                    self$status <- tryCatch(showStatus(as.character(self$getId())),
                                               warning = function(w) {
                                                   cat("Date twtid warning: ",paste(w));
                                                   print("");
                                               },
                                               error = function(e) {
                                                   cat("Date twtid error",self$getId()," ",paste(e));
                                                   print("");
                                               })
                      
                    get_env(connections)$addNumRequestToTwitter();
                    View(status)
                    stop()
                  
                },"")
                date = self$status$getCreated();
            
                format1 = "%Y-%m-%d %H:%M:%S %Z";
                date <- as.POSIXct(date,format = format1)
                format2 <- "%a %b %d %H:%M:%S %Z %Y"
                private$date <- format(date,format2)
                
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
        getId = function(){
          return(self$id);  
        },
        obtainSource = function(){

            if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".twtid",sep = ""))) {
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_" , self$getSpecificProperties("target") , "_/" , self$id , ".twtid",sep = "")
                
                private$source <- enc2utf8(readLines(private$path))
                ifelse((private$source == ""),{
                    mensaje <- c( "el archivo " , x$getPath() , " tiene el source vacio")
                    warning(mensaje)
                },"")
            }else{
                  ifelse((is.null(self$status)),{
                      get_env(connections)$checkRequestToTwitter();
                      
                      twitteR:::check_id(as.character(self$getId()))                     
                      self$status <- tryCatch(showStatus(as.character(self$getId())),
                                                 warning = function(w) {
                                                     cat("Source twtid warning: ",paste(w));
                                                     print("");
                                                 },
                                                 error = function(e) {
                                                     cat("Source twtid error",self$getId()," ",paste(e))
                                                     print("");
                                                 })

                      get_env(connections)$addNumRequestToTwitter();
                  },"");
                
                  private$source <- enc2utf8(self$status$getText());
                  cat(private$source,
                              file = paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_",
                                           self$getSpecificProperties("target"),"_/",
                                           self$id ,".twtid",sep = ""),sep = "\n"
                  )
            }
        }
    )
)