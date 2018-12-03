DataYtbid<- R6Class(
    classname = "DataYtbid",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
            self$obtainId();
            get_env(conexiones)$startConectionWithYoutube()
            
        },
        id = "",
        obtainDate = function(){
            
            if(file.exists(paste("content-preprocessor/cache/youtube/commentsLeidosDate/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".ytbid",sep = ""))){
                
                private$path <- paste("content-preprocessor/cache/youtube/commentsLeidosSource/_" , self$getSpecificProperties("target") , "_/" , self$id , ".ytbid",sep = "")
                private$date <- readLines(paste("content-preprocessor/cache/youtube/commentsLeidosDate/_",
                                                  self$getSpecificProperties("target"),"_/",
                                                self$getId() ,".ytbid",sep = ""))
            }else{
                #' When filter is code comment_id, and code simplify is code TRUE, and there is a correct comment id, 
                #' it returns a code data.frame with the following cols: 
                #' code id, authorDisplayName, authorProfileImageUrl, authorChannelUrl, value, textDisplay, canRate, viewerRating, likeCount
                #' publishedAt, updatedAt
                
                date <- tryCatch(levels(get_comments(filter = c(comment_id = self$getId()),textFormat = "plainText")[["publishedAt"]][["publishedAt"]]) ,
                                              warning = function(w) {
                                                  print("Date ytbid warning");
                                                  print("");
                                              },
                                              error = function(e) {
                                                  print(c("Date ytbid error",self$getId(),e));
                                                  print("");
                                              })
                
                date <- paste(substring(date,0,10),substring(date,12,nchar(date))," ")
                date <- as.POSIXct(date)
                formato <- "%a %b %d %H:%M:%S %Z %Y"
                private$date <- format(date,formato)
                
                cat(private$date,
                    file = paste("content-preprocessor/cache/youtube/commentsLeidosDate/_",
                                 self$getSpecificProperties("target"),"_/",
                                 self$id ,".ytbid",sep = ""),sep = "\n"
                )
            }

            #private$date<- get_comments(filter = c(comment_id = id),textFormat = plainText)[publishedAt]
        },
        obtainId = function(){
            self$id <- readLines(self$getPath(),warn=FALSE)
        },
        obtainSource = function(){
            if( file.exists(paste("content-preprocessor/cache/youtube/commentsLeidosSource/_",
                                 self$getSpecificProperties("target"),"_/",self$id,".ytbid",sep = ""))){
                
                private$path <- paste("content-preprocessor/cache/youtube/commentsLeidosSource/_" , self$getSpecificProperties("target") , "_/" , self$id , ".ytbid",sep = "")
                
                private$source <- readLines(paste("content-preprocessor/cache/youtube/commentsLeidosSource/_",
                                                  self$getSpecificProperties("target"),"_/",
                                                  self$id ,".ytbid",sep = ""))
                
            }else{
                #' When filter is code comment_id, and code simplify is code TRUE, and there is a correct comment id, 
                #' it returns a code data.frame with the following cols: 
                #' code id, authorDisplayName, authorProfileImageUrl, authorChannelUrl, value, textDisplay, canRate, viewerRating, likeCount
                #' publishedAt, updatedAt
                
                private$source <- tryCatch(levels(get_comments(filter = c(comment_id = self$getId()),textFormat = "plainText")[["textDisplay"]][["textDisplay"]])  ,
                                           warning = function(w) {
                                               print("Source ytbid warning");
                                               print("");
                                           },
                                           error = function(e) {
                                               print(c("Source ytbid error",self$getId(),e));
                                               print("");
                                           })
               
                cat(private$source,
                    file = paste("content-preprocessor/cache/youtube/commentsLeidosSource/_",
                                 self$getSpecificProperties("target"),"_/",
                                 self$id ,".ytbid",sep = ""),sep = "\n"
                )
            }

           # private$source <- get_comments(filter = c(comment_id = id),textFormat = plainText)[value]
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