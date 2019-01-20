#Class to handle twtid files
#
#It is a class that inherits from the Instance class and implements 
#the functions of extracting the text and the date of an twtid-type file
#
#Variables:
#id: (character) id of tweet
#status: (list) Information returned by the twitter api
#
ExtractorTwtid <- R6Class(
    
  classname = "ExtractorTwtid",
    
  inherit = Instance,
    
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
      connections$startConectionWithTwitter()

      ##Revisar porque en algunos archivos se obtiene un array de caracteres de 
      ##2 posiciones
      ## en vez de de solo una posicion
      # if (!(validUTF8(super$getSource())[1])){
      #     cat( "el archivo " , super$getPath() , " no es utf8")
      # }
      
      return()
    },

    obtainId = function() {
      #
      #Function that obtain the id of the twtid 
      #
      #Read the id of the file indicated in the variable path
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #   
      private$id <- readLines(super$getPath(), warn = FALSE, n = 1) 
      
      return()
    },
        
    getId = function() {
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
    
    getStatus = function() {
      #
      #Getter of status variable
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   value of status variable
      #        
      return(private$status)
    },
        
    obtainDate = function() {
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
      if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/",
             "tweetsLeidosDate/_", super$getSpecificProperty("target"), "_/", 
               self$getId(), ".twtid", sep = ""))) {
            
        private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/",
               "tweetsLeidosDate/_",
                 super$getSpecificProperty("target"), "_/", 
                   self$getId(), ".twtid", sep = "")
        
        
        super$getPath() %>>%
          readLines() %>>%
            super$setDate()
        
            
        if (super$getDate() == "") {
          cat("The file of twtid", super$getPath(), " has an empty date\n")
        }
        
      } else {  
            
        dateTwtid <- ""
            
        if (is.null(self$getStatus())) {
                
          connections$checkRequestToTwitter()
                
          self$getId() %>>%
            as.character() %>>%
              twitteR:::check_id()
                
          private$status <- tryCatch(
                                      self$getId() %>>%
                                        as.character() %>>%
                                          showStatus(),
                                       
                                      warning = function(w) {
                                        cat("Date twtid warning: ", paste(w))
                                        print("")
                                      },
                                       
                                      error = function(e) {
                                        cat("Date twtid error: ", self$getId(), 
                                               " ", paste(e))
                                        print("")
                                      }
                                    )
             
          if (!is.null(self$getStatus()) &&
               "status" %in% class(self$getStatus()) &&
                 length(self$getStatus()) > 0) {
            
            dateTwtid <- self$getStatus()$getCreated()
              
          } else {
            dateTwtid <- ""
          }
   
          connections$addNumRequestToTwitter()

        } else {
                
          if (!is.null(self$getStatus()) && 
               "status" %in% class(self$getStatus()) && 
                 length(self$getStatus()) > 0) {
              
            dateTwtid <- self$getStatus()$getCreated()
              
          } else {
            dateTwtid <- ""
          }
        }
            
        formatDateTwtid <- "%Y-%m-%d %H:%M:%S %Z"
        StandardizedDate <- as.POSIXct(dateTwtid, format = formatDateTwtid)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        format(StandardizedDate,formatDateGeneric) %>>%
          super$setDate()
        
        cat(as.character(private$date),
            file = paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                          "tweetsLeidosDate/_", 
                            self$getSpecificProperty("target"), "_/",
                              self$getId(), ".twtid", sep = ""), sep = "\n")
      }
      
      return()
    },
        
    obtainSource = function(){
      #
      #Function that obtain the source of the twtid id
      #
      #First check if the source is stored in cache, if it is not found, 
      #it checks if you need to make a request to twitter to return the status 
      #of the tweet. From the status, we obtain the source and keep it in cache.
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #  
      if (file.exists(
           paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                   "tweetsLeidosSource/_", 
                     super$getSpecificProperty("target"), "_/",
                       self$getId(), ".twtid", sep = ""))) {
            
        private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                               "tweetsLeidosSource/_", 
                                 super$getSpecificProperty("target"), "_/", 
                                   self$getId(), ".twtid", sep = "")
        
        super$getPath() %>>%    
          readLines() %>>%
            enc2utf8() %>>%
              super$setSource()  
        
        num <- 0
        
        super$getSource() %>>%
          {nchar(., type = "width")} %>>%
            num ~ sum() 
          
        
        if (length(num) > 1 &&  num == 0) {
          cat("The file of twtid " , super$getPath() , " has an empty date\n")
        }
            
        } else {
            
          if (is.null(self$getStatus())) {
            
            connections$checkRequestToTwitter()
              
            self$getId() %>>%
              as.character() %>>%
                twitteR:::check_id()
            
            private$status <- tryCatch(
                                        self$getId() %>>%
                                          as.character() %>>%
                                            showStatus(),
                                        
                                        warning = function(w) {
                                          cat("Date twtid warning: ", paste(w))
                                          print("")
                                        },
                                        
                                        error = function(e) {
                                          cat("Date twtid error: ", self$getId(), 
                                              " ", paste(e))
                                          print("")
                                        }
                                      )
              
            connections$addNumRequestToTwitter()

            
            if (!is.null(self$getStatus()) && 
                 "status" %in% class(self$getStatus()) &&
                   length(self$getStatus()) > 0) {
                  
              self$getStatus()$getText() %>>%
                enc2utf8() %>>%
                  super$setSource()
                  
            } else {
              super$setSource("")
            }
         
            cat(private$source, 
                  file = paste("content-preprocessorinr/testFiles/cache/",
                                "hsspam14/tweetsLeidosSource/_",
                                  super$getSpecificProperty("target"), "_/",
                                    self$getId(), ".twtid", sep = ""), 
                                      sep = "\n")
      
          } else {
            
            if (!is.null(self$getStatus()) && 
                 "status" %in% class(self$getStatus()) && 
                   length(self$getStatus()) > 0) {
              
              self$getStatus()$getText() %>>%
                enc2utf8() %>>%
                  super$setSource()
                
            } else {
              super$setSource("")
            }
           
            cat(private$source, 
                file = paste("content-preprocessorinr/testFiles/cache/",
                              "hsspam14/tweetsLeidosSource/_",
                                super$getSpecificProperty("target"), "_/",
                                  self$getId(), ".twtid", sep = ""), 
                                    sep = "\n")
          }
        }
      private$source %>>%
        super$setData()
      
      return()
    }
  ),
  
  private = list(
    id = "",
    status = NULL
  )
)