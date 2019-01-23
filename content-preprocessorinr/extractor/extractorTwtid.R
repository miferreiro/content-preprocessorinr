#Class to handle twtid files
#
#It is a class that inherits from the Instance class and implements 
#the functions of extracting the text and the date of an twtid-type file
#
#Variables:
#id: (character) id of tweet
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
    
    obtainDate = function() {
      #
      #Function that obtain the date of the twtid id
      #
      #
      #
      #
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #  
     
      
      if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/",
            "tweets/_", super$getSpecificProperty("target"), "_/", 
              self$getId(), ".json", sep = ""))) {
        
        private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
                                super$getSpecificProperty("target"), "_/", 
                                  self$getId(), ".json", sep = "")
  
        dataFromJsonFile <- fromJSON(file = private$path)
  
        if (!is.na(dataFromJsonFile[["date"]]) && 
              !is.null(dataFromJsonFile[["date"]]) 
                && dataFromJsonFile[["date"]] != "") {
          
          super$setDate(dataFromJsonFile[["date"]])
          return()
          
        } 
      }
      
      if (super$getDate() == "") {
        
        dateTwtid <- ""
        sourceTwtid <- ""
        langTwtid <- ""
        
        connections$checkRequestToTwitter()
        
        lookup <- tryCatch(  self$getId() %>>% 
                               as.character() %>>%
                                 rtweet::lookup_tweets(),
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
        
        if (!is.null(lookup) &&
            "tbl_df" %in% class(lookup) ) {

          dateTwtid <- lookup$created_at 
          sourceTwtid <- lookup$text
          langTwtid <- lookup$lang
          
        } else {
          dateTwtid <- ""
          sourceTwtid <- ""
          langTwtid <- ""
        }
        
        connections$addNumRequestToTwitter()
          
        formatDateTwtid <- "%Y-%m-%d %H:%M:%S %Z"
        StandardizedDate <- as.POSIXct(dateTwtid, format = formatDateTwtid)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        
        format(StandardizedDate,formatDateGeneric) %>>%
          enc2utf8() %>>%
            super$setDate()
       
        lista <- list(source = enc2utf8(sourceTwtid), 
                       date = enc2utf8(as.character(super$getDate())),
                         lang = enc2utf8(langTwtid))
        
        tryCatch(
          {
            exportJSON <- toJSON(lista)
            cat(exportJSON,
                file = paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                               "tweets/_", 
                                 self$getSpecificProperty("target"), "_/",
                                   self$getId(), ".json", sep = ""), sep = "\n")
          }
        ,
        error = function(e) {
          cat("Error exportJSON: ",e,"\n")
          lista <- list(source = "",
                          date = enc2utf8(as.character(super$getDate())),
                            lang = enc2utf8(langTwtid))
          
          exportJSON <- toJSON(lista)
          
          cat(exportJSON,
                file = paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                               "tweets/_", 
                                 self$getSpecificProperty("target"), "_/",
                                   self$getId(), ".json", sep = ""), sep = "\n")
        }
        )

      }
 
      return()
    },
        
    obtainSource = function(){
      #
      #Function that obtain the source of the twtid id
      #
      #
      #
      #
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #  
      
      if (file.exists(
            paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                    "tweets/_", super$getSpecificProperty("target"), "_/", 
                       self$getId(), ".json", sep = ""))) {
        
        private$path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
                                super$getSpecificProperty("target"), "_/", 
                                  self$getId(), ".json", sep = "")
        
        dataFromJsonFile <- fromJSON(file = private$path)
        
        if (!is.na(dataFromJsonFile[["source"]]) && 
              !is.null(dataFromJsonFile[["source"]]) && 
                dataFromJsonFile[["source"]] != "") {
        
          dataFromJsonFile[["source"]] %>>%
            enc2utf8() %>>%
              super$setSource()
          
          private$source %>>%
            super$setData()
          
          return()
        } 
      }
      
      if (super$getSource() == "") {
        
        dateTwtid <- ""
        sourceTwtid <- ""
        langTwtid <- ""
        
        connections$checkRequestToTwitter()
        
        lookup <- tryCatch(  self$getId() %>>% 
                               as.character() %>>%
                                rtweet::lookup_tweets(),
                             
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
        
        if (!is.null(lookup) &&
            "tbl_df" %in% class(lookup) ) {
          
          dateTwtid <- lookup$created_at 
          sourceTwtid <- lookup$text
          langTwtid <- lookup$lang
          
        } else {
          dateTwtid <- ""
          sourceTwtid <- ""
          langTwtid <- ""
        }
        
        connections$addNumRequestToTwitter()
        
        sourceTwtid %>>%
          enc2utf8() %>>%
            super$setSource()
        
        private$source %>>%
          super$setData()
        
        formatDateTwtid <- "%Y-%m-%d %H:%M:%S %Z"
        StandardizedDate <- as.POSIXct(dateTwtid, format = formatDateTwtid)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        
        dateTwtid <- enc2utf8(format(StandardizedDate,formatDateGeneric))
        
        lista <- list(source = super$getSource(), 
                        date = dateTwtid,
                          lang = enc2utf8(langTwtid))
        
        tryCatch(
          {
          
          exportJSON <- toJSON(lista)
          cat(exportJSON,
              file = paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                            "tweets/_", 
                              self$getSpecificProperty("target"), "_/",
                                self$getId(), ".json", sep = ""), sep = "\n")
          }
          ,
          error = function(e){
            print(e)
            lista <- list(source = "", 
                            date = enc2utf8(dateTwtid),
                              lang = enc2utf8(langTwtid))
            
            exportJSON <- toJSON(lista)
            
            cat(exportJSON,
                file = paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                              "tweets/_", 
                                self$getSpecificProperty("target"), "_/",
                                  self$getId(), ".json", sep = ""), sep = "\n")
          }
        )
      }      

      return()
    }
  ),
  
  private = list(
    id = ""
  )
)