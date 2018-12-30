#Class to handle ytbid files
#
#It is a class that inherits from the ExtractorSource class and implements 
#the functions of extracting the text and the date of an ytbid-type file
#
#Variables:
#id: (character) id of comment
#comment: (list) Information returned by the youtube api
#
ExtractorYtbid <- R6Class(
    
  classname = "ExtractorYtbid",
    
  inherit = ExtractorSource,
    
  public = list(
        
    initialize = function(path) {
      #
      #Class constructor 
      #
      #This constructor calls the constructor of the superclass to which 
      #it passes the path of the file. In addition, obtains the ID of the tweet 
      #of the file indicated in the path.
      #In the end, establish a connection with twitter, as long as it has not 
      #already been established.
      #
      #Args: 
      #   path: (character) Path of the twtid-type file
      #
      #Returns: 
      #   null
      #                 
      super$initialize(path)
      self$obtainId()
      #Singleton
      connections$startConectionWithYoutube()
      
      ##Revisar porque en algunos archivos se obtiene un array de caracteres de 2 posiciones
      ## en vez de de solo una posicion
      # if(!(validUTF8(super$getSource())[1])){
      #     cat( "el archivo " , super$getPath() , " no es utf8")
      # }
      
      return()
    },
    
    obtainId = function(){
      #
      #Function that obtain the id of the ytbid 
      #
      #Read the id of the file indicated in the variable path
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #   
      private$id <- readLines(super$getPath(),warn = FALSE)
      
      return()
    },    
    
    getId = function(){
      #
      #Getter of id variable
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   value of id variable
      #        
      return(private$id)
    },

    getComment = function(){
      #
      #Getter of comment variable
      #
      ##' When filter is code comment_id, and code simplify is code TRUE, and there is a correct comment id, 
      #' it returns a code data.frame with the following cols: 
      #' code id, authorDisplayName, authorProfileImageUrl, authorChannelUrl, value, textDisplay, canRate, viewerRating, likeCount
      #' publishedAt, updatedAt
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   value of comment variable
      #        
      return(private$comment)
    },        

    obtainDate = function(){
      #
      #Function that obtain the date of the twtid id
      #
      #First check if the date is stored in cache, if it is not found, 
      #it checks if you need to make a request to twitter to return the status 
      #of the tweet. From the status, we obtain the date and keep it in cache.
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #               
      if (file.exists(paste("content-preprocessorinr/testFiles/cache/youtube/",
              "commentsLeidosDate/_", super$getSpecificProperty("target"), "_/",
                self$getId(), ".ytbid", sep = ""))) {
            
        private$path <- paste("content-preprocessorinr/testFiles/cache/youtube/",
               "commentsLeidosDate/_", 
                 super$getSpecificProperty("target"), "_/",
                   self$getId(), ".ytbid", sep = "")
        
        super$getPath() %>>%
          readLines() %>>%
            super$setDate()

        if (super$getDate() == "") { 
          cat( "The file of ytbid " , super$getPath() , " has an empty date\n")
        }
            
      } else {
        dateYtbid <- ""
            
        if (is.null(self$getComment())) {
                
          connections$checkRequestToYoutube()
                    
          private$comment <- tryCatch(
                                       get_comments(
                                         filter = c(comment_id = self$getId()), 
                                         textFormat = "plainText"),  
                                     
                                       warning = function(w) {
                                         cat("Date ytbid warning ", paste(w))
                                         print("")
                                       },
                                      
                                       error = function(e) {
                                         cat("Date ytbid error ", self$getId()
                                             , " ", paste(e))
                                         print("")
                                       }
                                     )
                
          connections$addNumRequestToYoutube()
                
          if ( self$getComment() != "" || length(self$getComment()) > 1 ) {
            dateYtbid  <- levels(self$getComment()[["publishedAt"]][["publishedAt"]]) 
              
          } else {
            dateYtbid <- ""
          }
                
        } else {
                
          if (self$getComment() != "" || length(self$getComment()) > 1) {
            dateYtbid <- levels(self$getComment()[["publishedAt"]][["publishedAt"]]) 
              
          } else {
            dateYtbid <- ""
          }
        }
            
        if ( dateYtbid != "") {
          dateYtbid <- paste(substring(dateYtbid, 0, 10), 
                             substring(dateYtbid, 12, nchar(dateYtbid)), " ")

          StandardizedDate <- tryCatch(
                                 as.POSIXct(dateYtbid),
                                 
                                 warning = function(w) {
                                     cat("Date ytbid warning as.POSIXct: ",
                                         paste(w))
                                     print("")
                                 },
                                 
                                 error = function(e) {
                                     cat("Date ytbid error as.POSIXct ", 
                                         self$getId(), " ", paste(e))
                                     print("")
                                 }
                               )
                           
          formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
          format(StandardizedDate,formatDateGeneric) %>>%
            super$setDate()
          
                
          cat(private$date,
              file = paste("content-preprocessorinr/testFiles/cache/youtube/",
                           "commentsLeidosDate/_",
                           self$getSpecificProperty("target"), "_/",
                           self$getId(), ".ytbid", sep = ""), sep = "\n")
        } else {
          private$date <- ""
        }
      }
    },
    
    obtainSource = function(){
      #
      #Function that obtain the source of the ytbid id
      #
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #          
      if (file.exists(paste("content-preprocessorinr/testFiles/cache/youtube/",
                              "commentsLeidosSource/_",
                                  super$getSpecificProperty("target"), "_/", 
                                    self$getId(), ".ytbid", sep = ""))) {
            
        private$path <- paste("content-preprocessorinr/testFiles/cache/youtube/",
                                "commentsLeidosSource/_", 
                                  super$getSpecificProperty("target"), "_/", 
                                    self$getId() , ".ytbid", sep = "")
           
        super$getPath() %>>%    
          readLines() %>>%
            enc2utf8() %>>%
              super$setSource()  
            
        num <- 0
        
        super$getSource() %>>%
          {nchar(., type = "width")} %>>%
            num ~ sum() 
        
        
        if (length(num) > 1 &&  num == 0) {
          cat("The file of ytbid " , super$getPath() , " has an empty date\n")
        }
            
      } else {
        if (is.null(self$getComment())) { 
                
          connections$checkRequestToYoutube()
               
          self$comment <- tryCatch( 
                                    get_comments(
                                      filter = c(comment_id = self$getId()),
                                      textFormat = "plainText"), 
                                   
                                    warning = function(w) {
                                      cat("Source ytbid warning ", paste(w))
                                      print("")
                                    },
                                   
                                    error = function(e) {
                                      cat("Source ytbid error ", self$getId(), 
                                           " ", paste(e))
                                      print("")
                                    }
                                  )
            
          if (self$getComment() != "" || length(self$getComment()) > 1) {
            private$source  <- enc2utf8(
                    levels(self$getComment()[["textDisplay"]][["textDisplay"]])) 
          } else {
              private$source <- ""
          }
                
          connections$addNumRequestToYoutube()
          
          cat(super$getSource(),
              file = paste("content-preprocessorinr/testFiles/cache/youtube/",
                             "commentsLeidosSource/_",
                               super$getSpecificProperty("target"), "_/",
                                 self$getId(), ".ytbid", sep = ""), sep = "\n")
        } else {
                
          if (self$getComment() != "" || length(self$getComment()) > 1) {
            private$source  <- enc2utf8(
              levels(self$getComment()[["textDisplay"]][["textDisplay"]])) 
          }else{
            private$source <- ""
          }
          cat(super$getSource(),
              file = paste("content-preprocessorinr/testFiles/cache/youtube/",
                             "commentsLeidosSource/_",
                                super$getSpecificProperty("target"), "_/",
                                  self$getId(), ".ytbid", sep = ""), sep = "\n")
        }
      }
      
      private$source %>>%
        super$setData()
      
      return()
    }
  ),
  
  private = list(
    id = "",
    comment = NULL
  )
)