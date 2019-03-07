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
      path %>>%
        super$initialize()
      
      self$obtainId()
      #Singleton
      connections$startConnectionWithTwitter()
      
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
      #Check if the tweet has previously been cached. In this case, the file is 
      #read in json format and the date is stored. Otherwise, the request is made
      #on twitter. The date is then formatted to the established standard.
      #
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
      
      if (file.exists(
        paste(
          "content-preprocessorinr/testFiles/cache/hsspam14/",
          "tweets/_",
          super$getSpecificProperty("target"),
          "_/",
          self$getId(),
          ".json",
          sep = ""
        )
      )) {
        
        private$path <-
          paste(
            "content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          )
        
        dataFromJsonFile <- rjson::fromJSON(file = private$path)
        
        if (!is.na(dataFromJsonFile[["date"]]) &&
              !is.null(dataFromJsonFile[["date"]]) &&
                dataFromJsonFile[["date"]] != "") {
          
          dataFromJsonFile[["date"]] %>>%
            super$setDate()
          
          return()
        }
      }
      
      if (super$getDate() == "") {
        
        dateTwtid <- ""
        sourceTwtid <- ""
        langTwtid <- ""
        
        connections$checkRequestToTwitter()

        lookup <- tryCatch(
          
          self$getId() %>>%
            as.character() %>>%
              rtweet::lookup_tweets(.,p = connections$getTwitterToken()),
          
          warning = function(w) {
            cat(paste("[ExtractorTwtid][obtainDate][Warning] Date twtid warning: ",
                      self$getId(), " ", paste(w)),"\n")
          },
          
          error = function(e) {
            cat(paste("[ExtractorTwtid][obtainDate][Error] Date twtid error: ",
                      self$getId(), " ", paste(w)),"\n")
          }
        )
        
        if (!is.null(lookup) &&
            "tbl_df" %in% class(lookup)) {
          
          dateTwtid <- lookup$created_at
          sourceTwtid <- lookup$text
          langTwtid <- lookup$lang
          
        } else {
          dateTwtid <- ""
          sourceTwtid <- ""
          langTwtid <- ""
        }
        
        formatDateTwtid <- "%Y-%m-%d %H:%M:%S %Z"
        StandardizedDate <-
          as.POSIXct(dateTwtid, format = formatDateTwtid)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        
        format(StandardizedDate, formatDateGeneric) %>>%
          as.character() %>>%
              super$setDate()
        
        lista <- list(
          source = sourceTwtid,
          date = super$getDate(),
          lang = langTwtid
        )
        
        tryCatch({
          
          exportJSON <- rjson::toJSON(lista)
          
          cat(
            exportJSON,
            file = paste(
              "content-preprocessorinr/testFiles/cache/hsspam14/",
              "tweets/_",
              super$getSpecificProperty("target"),
              "_/",
              self$getId(),
              ".json",
              sep = ""
            ),
            sep = "\n"
          )
        },
        
        error = function(e) {
          cat(paste("[ExtractorTwtid][obtainDate][Error] exportJSON: ",
                      self$getId(), " " , paste(e), "\n"))
          
          lista <- list(source = "",
                        date = super$getDate(),
                        lang = langTwtid)
          
          exportJSON <- rjson::toJSON(lista)
          
          cat(
            exportJSON,
            file = paste(
              "content-preprocessorinr/testFiles/cache/hsspam14/",
              "tweets/_",
              super$getSpecificProperty("target"),
              "_/",
              self$getId(),
              ".json",
              sep = ""
            ),
            sep = "\n"
          )
        })
      }
      
      return()
    },
    
    obtainSource = function() {
      #
      #Function that obtain the source of the twtid id
      #
      #Check if the tweet has previously been cached. In this case, the file is 
      #read in json format and the source is stored. Otherwise, the request is made
      #on twitter. 
      #
      #Args:
      #   null
      #
      #Returns:
      #   null
      #
      
      if (file.exists(
        paste(
          "content-preprocessorinr/testFiles/cache/hsspam14/",
          "tweets/_",
          super$getSpecificProperty("target"),
          "_/",
          self$getId(),
          ".json",
          sep = ""
        )
      )) {
        private$path <-
          paste(
            "content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          )
        
        dataFromJsonFile <- rjson::fromJSON(file = super$getPath())
        
        if (!is.na(dataFromJsonFile[["source"]]) &&
              !is.null(dataFromJsonFile[["source"]]) &&
                dataFromJsonFile[["source"]] != "") {
          
          dataFromJsonFile[["source"]] %>>%
              super$setSource()
          
          super$getSource() %>>%
            super$setData()
          
          return()
        }
      }

  
      if (super$getSource() == "") {
        dateTwtid <- ""
        sourceTwtid <- ""
        langTwtid <- ""
        
        connections$checkRequestToTwitter()
        
        lookup <- tryCatch(
          self$getId() %>>%
            as.character() %>>%
              rtweet::lookup_tweets(.,p = connections$getTwitterToken()),
          
          warning = function(w) {
            cat(paste("[ExtractorTwtid][obtainSource][Warning] Source twtid warning: ",
                      self$getId(), " ", paste(w)),"\n")
          },
          
          error = function(e) {
            cat(paste("[ExtractorTwtid][obtainSource][Error] Source twtid error: ",
                      self$getId(), " ", paste(e)),"\n")
          }
        )
        
        if (!is.null(lookup) &&
            "tbl_df" %in% class(lookup)) {
          
          dateTwtid <- lookup$created_at
          sourceTwtid <- lookup$text
          langTwtid <- lookup$lang
          
        } else {
          dateTwtid <- ""
          sourceTwtid <- ""
          langTwtid <- ""
        }
        
        sourceTwtid %>>%
          super$setSource()
        
        super$getSource() %>>%
          super$setData()
        
        formatDateTwtid <- "%Y-%m-%d %H:%M:%S %Z"
        StandardizedDate <-
          as.POSIXct(dateTwtid, format = formatDateTwtid)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        
        dateTwtid <- format(StandardizedDate, formatDateGeneric)
        
        lista <- list(
          source = super$getSource(),
          date = as.character(dateTwtid),
          lang = langTwtid
        )
        
        
        tryCatch({
          
          exportJSON <- rjson::toJSON(lista)
          cat(
            exportJSON,
            file = paste(
              "content-preprocessorinr/testFiles/cache/hsspam14/",
              "tweets/_",
              super$getSpecificProperty("target"),
              "_/",
              self$getId(),
              ".json",
              sep = ""
            ),
            sep = "\n"
          )
        },
        error = function(e) {

          cat(paste("[ExtractorTwtid][obtainSource][Error] exportJSON: ",
                    self$getId(), " " , paste(e), "\n"))
          
          lista <- list(
            source = "",
            date = as.character(dateTwtid),
            lang = langTwtid
          )
          
          exportJSON <- rjson::toJSON(lista)
          
          cat(
            exportJSON,
            file = paste(
              "content-preprocessorinr/testFiles/cache/hsspam14/",
              "tweets/_",
              super$getSpecificProperty("target"),
              "_/",
              self$getId(),
              ".json",
              sep = ""
            ),
            sep = "\n"
          )
        })
      }
      
      return()
    }
  ),
  
  private = list(
    id = ""
  )
)