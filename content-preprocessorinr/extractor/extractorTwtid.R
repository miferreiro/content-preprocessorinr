ExtractorTwtid <- R6Class(
    
    classname = "ExtractorTwtid",
    
    inherit = ExtractorSource,
    
    public = list(
        
        initialize = function(path) {
           
            super$initialize(path)
            self$obtainId()
            #Se comprueba si se ha conetactdo con twitter.
            #En el caso de que no se haya conectado, se conecta.
            #Singleton
            connections$startConectionWithTwitter()
            # super$addProperties(generalFun$getTarget(super$getPath()),"target")
            # super$obtainSourceDate()
            
            ##Revisar porque en algunos archivos se obtiene un array de caracteres de 2 posiciones
            ## en vez de de solo una posicion
            # if (!(validUTF8(super$getSource())[1])){
            #     cat( "el archivo " , super$getPath() , " no es utf8")
            # }
            
        },
        
        id = "",
        
        status = NULL,
        
        obtainDate = function(){
            
            if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosDate/_",
                                  super$getSpecificProperties("target"),"_/",self$getId(),".twtid",sep = ""))){
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosDate/_" 
                                      , super$getSpecificProperties("target") , "_/" , self$getId() , ".twtid",sep = "")
                
                private$date <- readLines(super$getPath())
                
                if (private$date == ""){
                    cat( "el archivo " , super$getPath() , " tiene la fecha vacia")
                    
                }
            }else{  
                
                date = "";
                
                if (is.null(self$status)) {
                    
                    connections$checkRequestToTwitter();
                    
                    twitteR:::check_id(as.character(self$getId()))
                    
                    self$status <- tryCatch(
                                            {
                                                showStatus(as.character(self$getId()));
                                            },
                                            warning = function(w) {
                                               cat("Date twtid warning: ",paste(w));
                                               print("");
                                            },
                                            error = function(e) {
                                               cat("Date twtid error",self$getId()," ",paste(e));
                                               print("");
                                            }
                                           )
                 
                    if ( !is.null(self$status) && "status" %in% class(self$status) 
                             && length(self$status) > 0)
                    {
                        date = self$status$getCreated();
                        
                    }else{
                        
                        date = "";
                        
                    }
             
                    connections$addNumRequestToTwitter();

                }else{
                    
                    if ( !is.null(self$status) && "status" %in% class(self$status) 
                             && length(self$status) > 0)
                    {
                        
                        date = self$status$getCreated();
                        
                    }else{
                        
                        date = "";
                    }
                }
                
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
            self$id <- readLines(super$getPath(),warn = FALSE, n = 1)
        },
        
        getId = function(){
          return(self$id);  
        },
        
        obtainSource = function(){

            if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_",
                                 super$getSpecificProperties("target"),"_/",self$getId(),".twtid",sep = ""))) {
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_" 
                                      , super$getSpecificProperties("target") , "_/" , self$getId() , ".twtid",sep = "")
                
                private$source <- enc2utf8(readLines(super$getPath()))
                num <- sum(nchar(super$getSource(), type = "width"))
                
                if (length(num) > 1 &&  num == 0) {
                    cat("el archivo " , super$getPath() , " tiene el source vacio","\n")
                }else{
                    if (length(num) > 1  ) {
                        
                    }
                }
                
                
            }else{
                
                  if (is.null(self$status)) {
                      connections$checkRequestToTwitter();
                      
                      twitteR:::check_id(as.character(self$getId()))                     
                      self$status <- 
                                    tryCatch(
                                             {
                                                  showStatus(as.character(self$getId()))
                                             },
                                             warning = function(w) {
                                                 cat("Source twtid warning: ",paste(w));
                                                 print("")
                                             },
                                             error = function(e) {
                                                 cat("Source twtid error",self$getId()," ",paste(e))
                                                 print("")
                                             }
                                            )
                      
                      connections$addNumRequestToTwitter();

                    
                      if ( !is.null(self$status) && "status" %in% class(self$status) 
                           && length(self$status) > 0 )
                      {
                          
                          private$source <- enc2utf8(x = self$status$getText());
                          
                      }else{
                          
                          private$source = "";
                          
                      }
                 
                      cat(private$source,
                          file = paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_",
                                       super$getSpecificProperties("target"),"_/",
                                       self$getId() ,".twtid",sep = ""),sep = "\n"
                      )
              
                  }else{
                      if ( !is.null(self$status) && "status" %in% class(self$status)
                           && length(self$status) > 0 ) 
                      {
                          
                          private$source <- enc2utf8(x = self$status$getText());
                          
                      }else{
                          
                          private$source = "";
                          
                      }
                     
                      cat(private$source,
                          file = paste("content-preprocessorinr/testFiles/cache/hsspam14/tweetsLeidosSource/_",
                                       super$getSpecificProperties("target"),"_/",
                                       self$getId() ,".twtid",sep = ""),sep = "\n")
                  }
            }
            self$setData(private$source)
        }
    )
)