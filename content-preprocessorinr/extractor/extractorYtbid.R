ExtractorYtbid <- R6Class(
    
    classname = "ExtractorYtbid",
    
    inherit = ExtractorSource,
    
    public = list(
        
        initialize = function(path) {
            
            super$initialize(path)
            self$obtainId();
            #Se comprueba si se ha conetactdo con youtube
            #En el caso de que no se haya conectado, se conecta.
            #Singleton
            connections$startConectionWithYoutube()
            
            ##Revisar porque en algunos archivos se obtiene un array de caracteres de 2 posiciones
            ## en vez de de solo una posicion
            # if(!(validUTF8(super$getSource())[1])){
            #     cat( "el archivo " , super$getPath() , " no es utf8")
            # }
        },
        
        id = "",
        
        comment = NULL,
        
        obtainDate = function(){
            
            if (file.exists(paste("content-preprocessorinr/testFiles/cache/youtube/commentsLeidosDate/_",
                                 super$getSpecificProperties("target"),"_/",self$getId() ,".ytbid",sep = ""))) 
            {
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/youtube/commentsLeidosDate/_" 
                                      , self$getSpecificProperties("target") , "_/" , self$getId() , ".ytbid",sep = "")
                private$date <- readLines(super$getPath())

                if (private$date == "") { 
                    
                    cat( "el archivo " , super$getPath() , " tiene la fecha vacia")
                    
                }
                
            }else{
                #' When filter is code comment_id, and code simplify is code TRUE, and there is a correct comment id, 
                #' it returns a code data.frame with the following cols: 
                #' code id, authorDisplayName, authorProfileImageUrl, authorChannelUrl, value, textDisplay, canRate, viewerRating, likeCount
                #' publishedAt, updatedAt
                
                date = ""; 
                
                if (is.null(self$comment)) {
                    
                    connections$checkRequestToYoutube();
                        
                    self$comment <- tryCatch({
                                                get_comments(filter = c(comment_id = self$getId()),textFormat = "plainText")  
                                             },
                                              warning = function(w) {
                                                  cat("Date ytbid warning ", paste(w));
                                                  print("");
                                              },
                                              error = function(e) {
                                                  cat("Date ytbid error ", self$getId()," ",paste(e));
                                                  print("");
                                              }
                                             )
                    
                    connections$addNumRequestToYoutube();
                    
                    if ( self$comment != "" || length(self$comment) > 1 ) 
                    {
                        date = levels(self$comment[["publishedAt"]][["publishedAt"]])
                        
                    }else{
                        
                        date = "";
                        
                    }
                    
                }else{
                    
                    if (self$comment != "" || length(self$comment) > 1) 
                    {
                        
                         date = levels(self$comment[["publishedAt"]][["publishedAt"]])
                     
                    }else{
                        
                        date = "";
                        
                    }
                }
                
                if ( date != "") 
                {
                    
                    date <- paste(substring(date,0,10),substring(date,12,nchar(date))," ")
    
                    date <- tryCatch(
                                     {
                                         as.POSIXct(date)
                                     },
                                     warning = function(w) {
                                         cat("Date ytbid warning as.POSIXct: ", paste(w));
                                         print("");
                                     },
                                     error = function(e) {
                                         cat("Date ytbid error as.POSIXct ", self$getId()," ",paste(e));
                                         print("");
                                     }
                                    )
                                     
                    formato <- "%a %b %d %H:%M:%S %Z %Y"
                    private$date <- format(date,formato)
                    
                    cat(private$date,
                        file = paste("content-preprocessorinr/testFiles/cache/youtube/commentsLeidosDate/_",
                                     self$getSpecificProperties("target"),"_/",
                                     self$id ,".ytbid",sep = ""),sep = "\n"
                    )
               }else{
                   
                   private$date = "";
                   
               }
            }
        },
        
        obtainId = function(){
            self$id <- readLines(super$getPath(),warn = FALSE)
        },
        
        obtainSource = function(){
            
            if (file.exists(paste("content-preprocessorinr/testFiles/cache/youtube/commentsLeidosSource/_",
                                 self$getSpecificProperties("target"),"_/",self$getId(),".ytbid",sep = ""))) 
            {
                
                private$path <- paste("content-preprocessorinr/testFiles/cache/youtube/commentsLeidosSource/_" 
                                      , self$getSpecificProperties("target") , "_/" , self$id , ".ytbid",sep = "")
               
                private$source <- enc2utf8(readLines(super$getPath()))
                
                num <- sum(nchar(super$getSource(), type = "width"))
                
                if (length(num) > 1 &&  num == 0) 
                {
                    cat("el archivo " , super$getPath() , " tiene el source vacio","\n")
                    
                }else{
                    
                    if (length(num) > 1  ) {
                        
                    }
                }
                
            }else{
                #' When filter is code comment_id, and code simplify is code TRUE, and there is a correct comment id, 
                #' it returns a code data.frame with the following cols: 
                #' code id, authorDisplayName, authorProfileImageUrl, authorChannelUrl, value, textDisplay, canRate, viewerRating, likeCount
                #' publishedAt, updatedAt
                if (is.null(self$comment)) { 
                    
                    connections$checkRequestToYoutube();
                   
                    self$comment <- tryCatch( 
                                             {
                                                 get_comments(filter = c(comment_id = self$getId()),textFormat = "plainText") 
                                             },
                                             warning = function(w) {
                                                 cat("Source ytbid warning ", paste(w));
                                                 print("");
                                             },
                                             error = function(e) {
                                                 cat("Source ytbid error ", self$getId()," ",paste(e));
                                                 print("");
                                             }
                                            );
                
                    if (self$comment != "" || length(self$comment) > 1) 
                    {
                        private$source <- enc2utf8(levels(self$comment[["textDisplay"]][["textDisplay"]]));
                        
                    }else{
                        
                        private$source = "";
                        
                    }
                    
                    connections$addNumRequestToYoutube();
                    
                    cat(private$source,
                        file = paste("content-preprocessorinr/testFiles/cache/youtube/commentsLeidosSource/_",
                                     super$getSpecificProperties("target"),"_/",
                                     self$id ,".ytbid",sep = ""),sep = "\n"
                    );
                }else{
                    
                    if (self$comment != "" || length(self$comment) > 1)
                    {
                        private$source <- enc2utf8(levels(self$comment[["textDisplay"]][["textDisplay"]]));
                        
                    }else{
                        
                        private$source = "";
                        
                    }
                    cat(private$source,
                        file = paste("content-preprocessorinr/testFiles/cache/youtube/commentsLeidosSource/_",
                                     super$getSpecificProperties("target"),"_/",
                                     self$id ,".ytbid",sep = ""),sep = "\n");
                }
            }
            self$setData(private$source)
            
        },
        
        getId = function(){
            return(self$id);
        }
    )
)