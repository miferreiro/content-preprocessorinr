#Class to handle ytbid files
#
#It is a class that inherits from the Instance class and implements
#the functions of extracting the text and the date of an ytbid-type file
#
#Variables:
#id: (character) id of comment
#
ExtractorYtbid <- R6Class(
  
  classname = "ExtractorYtbid",
  
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
      connections$startConectionWithYoutube()
      
      return()
    },
    
    obtainId = function() {
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
      
      if (file.exists(
        paste(
          "content-preprocessorinr/testFiles/cache/youtube/",
          "comments/_",
          super$getSpecificProperty("target"),
          "_/",
          self$getId(),
          ".json",
          sep = ""
        )
      )) {
        
        private$path <-
          paste(
            "content-preprocessorinr/testFiles/cache/youtube/",
            "comments/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          )
        
        dataFromJsonFile <- fromJSON(file = super$getPath())
        
        
        if (!is.na(dataFromJsonFile[["date"]]) &&
              !is.null(dataFromJsonFile[["date"]]) &&
                dataFromJsonFile[["date"]] != "") {
          
          super$setDate(dataFromJsonFile[["date"]])
          return()
          
        }
      }
      
      
      if (super$getDate() == "") {
        
        dateYtbid <- ""
        sourceYtbid <- ""
        
        connections$checkRequestToYoutube()
        
        comment <- tryCatch(
          
          get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),
          
          warning = function(w) {
            warning(paste("Date ytbid warning ", paste(w)))
            print("")
          },
          
          error = function(e) {
            warning(paste("Date ytbid error ", self$getId(), " ", paste(e)))
            print("")
          }
        )
      }
      
      connections$addNumRequestToYoutube()
      
      if (comment != "" || length(comment) > 1) {
        
        dateYtbid  <- levels(comment[["publishedAt"]][["publishedAt"]])
        sourceYtbid <- levels(comment[["textDisplay"]][["textDisplay"]])
        
      } else {
        dateYtbid <- ""
        sourceYtbid <- ""
      }
      
      if (dateYtbid != "") {
        
        dateYtbid <- paste(substring(dateYtbid, 0, 10),
                            substring(dateYtbid, 12, nchar(dateYtbid)),
                              " ")
        
        StandardizedDate <- tryCatch(
          
          as.POSIXct(dateYtbid),
          
          warning = function(w) {
            warning(paste("Date ytbid warning as.POSIXct: ", w))
            print("")
          },
          
          error = function(e) {
            warning(paste("Date ytbid error as.POSIXct ",self$getId(), " ", e))
            print("")
          }
        )
        
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        format(StandardizedDate, formatDateGeneric) %>>%
          as.character() %>>%
            super$setDate()
        
      } else {
        super$setDate("")
        sourceYtbid <- ""
      }
      
      lista <- list(source = sourceYtbid,
                    date = super$getDate())
      
      tryCatch({
        
        exportJSON <- toJSON(lista)
        
        cat(
          exportJSON,
          file = paste(
            "content-preprocessorinr/testFiles/cache/youtube/",
            "comments/_",
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
        
        cat("toJSON:", e, "\n")
        
        lista <- list(source = "",
                      date = super$getDate())
        
        exportJSON <- toJSON(lista)
        
        cat(
          exportJSON,
          file = paste(
            "content-preprocessorinr/testFiles/cache/youtube/",
            "comments/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          ),
          sep = "\n"
        )
      })
      
      return()
    },
    
    obtainSource = function() {
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
      
      if (file.exists(
        paste(
          "content-preprocessorinr/testFiles/cache/youtube/",
          "comments/_",
          super$getSpecificProperty("target"),
          "_/",
          self$getId(),
          ".json",
          sep = ""
        )
      )) {
        private$path <-
          paste(
            "content-preprocessorinr/testFiles/cache/youtube/",
            "comments/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          )
        
        
        dataFromJsonFile <- fromJSON(file = super$getPath())
        
        
        if (!is.na(dataFromJsonFile[["source"]]) &&
              !is.null(dataFromJsonFile[["source"]]) &&
                dataFromJsonFile[["source"]] != "") {
          
          super$setSource(dataFromJsonFile[["source"]])
          super$getSource() %>>%
            super$setData()
          
          return()
        }
      }
      
      if (super$getSource() == "") {
        
        dateYtbid <- ""
        sourceYtbid <- ""
        
        connections$checkRequestToYoutube()
        
        comment <- tryCatch(
          
          get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),
          
          warning = function(w) {
            warning(paste("Date ytbid warning ", paste(w)))
            print("")
          },
          
          error = function(e) {
            warning(paste("Date ytbid error ", self$getId(), " ", paste(e)))
            print("")
          }
        )
      }
      
      connections$addNumRequestToYoutube()
      
      if (comment != "" || length(comment) > 1) {
        
        dateYtbid  <- levels(comment[["publishedAt"]][["publishedAt"]])
        sourceYtbid <- levels(comment[["textDisplay"]][["textDisplay"]])
        
      } else {
        dateYtbid <- ""
        sourceYtbid <- ""
      }
      
      if (dateYtbid != "") {
        
        dateYtbid <- paste(substring(dateYtbid, 0, 10),
                           substring(dateYtbid, 12, nchar(dateYtbid)),
                           " ")
        
        StandardizedDate <- tryCatch(
          as.POSIXct(dateYtbid),
          
          warning = function(w) {
            warning(paste("Date ytbid warning as.POSIXct: ", paste(w)))
            print("")
          },
          
          error = function(e) {
            warning(paste("Date ytbid error as.POSIXct ", self$getId(), " ", e))
            print("")
          }
        )
        
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        
        dateYtbid <-
            as.character(format(StandardizedDate, formatDateGeneric)
            )
        
      }
      
      sourceYtbid %>>%
          super$setSource()
      
      super$getSource() %>>%
        super$setData()
      
      lista <- list(source = super$getSource(),
                    date = dateYtbid)
      
      tryCatch({
        
        exportJSON <- toJSON(lista)
        
        cat(
          exportJSON,
          file = paste(
            "content-preprocessorinr/testFiles/cache/youtube/",
            "comments/_",
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
        
        cat("exportJSON", e, "\n")
        lista <- list(source = "", date = dateYtbid)
        exportJSON <- toJSON(lista)
        
        cat(
          exportJSON,
          file = paste(
            "content-preprocessorinr/testFiles/cache/youtube/",
            "comments/_",
            super$getSpecificProperty("target"),
            "_/",
            self$getId(),
            ".json",
            sep = ""
          ),
          sep = "\n"
        )
      })
      
      return()
    }
  ),
  
  private = list(id = "")
)