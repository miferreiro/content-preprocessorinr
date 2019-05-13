#' @title Class to handle ytbid files
#' @description It is a class that inherits from the \code{Instance} class and
#' implements the functions of extracting the text and the date of an ytbid-type
#' file.
#' @docType class
#' @usage ExtractorTwtid$new(path)
#' @param path  (character) Path of the ytbid-type file.
#' @details The connection to twitter is handled through the \code{Connections}
#' class, which needs a configuration file with the necessary keys to make
#' requests to the youtube API.
#'
#' This class stores in the folder testFiles/cache/youtube/comments the
#' comments processed so far, thus allowing you to save youtuve queries.
#' The text fields and the date of the comment are stored.
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainId}}{
#' Function that obtains the id of the ytbid. Read the id of the file indicated
#' in the variable path.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainId()}
#' }
#' }
#' }
#'
#' \item{\bold{getId}}{
#' Getter of comment id.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getId()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of comment id.
#' }
#' }
#' }
#'
#' \item{\bold{obtainDate}}{
#' Function that obtains the date of the ytbid id. Check if the comment has
#' previously been cached. In this case, the file is read in json format and the
#' date is stored. Otherwise, the request is made on youtube The date is then
#' formatted to the established standard.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainDate()}
#' }
#' }
#' }
#'
#' \item{\bold{obtainSource}}{
#' Function that obtains the source of the twtid id. Check if the tweet has
#' previously been cached. In this case, the file is read in json format and the
#' source is stored. Otherwise, the request is made on twitter.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainSource()}
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{id}}{
#'  (character) id of comment.
#' }
#' }
#'
#' @seealso \code{\link{Instance}}
#'
#' @import R6 pipeR rjson tuber
#' @export ExtractorYtbid

ExtractorYtbid <- R6Class(
  
  classname = "ExtractorYtbid",
  
  inherit = Instance,
  
  public = list(
    
    initialize = function(path) {

      path %>>%
        super$initialize()
      
      self$obtainId()
      #Singleton
      Bdp4R[["private_fields"]][["connections"]]$startConnectionWithYoutube()
      
      return()
    },
    
    obtainId = function() {

      private$id <- readLines(super$getPath(), warn = FALSE, n = 1)
      
      return()
    },
    
    getId = function() {

      return(private$id)
    },
    
    obtainDate = function() {

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
        
        dataFromJsonFile <- rjson::fromJSON(file = super$getPath())
        
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
        
        Bdp4R[["private_fields"]][["connections"]]$checkRequestToYoutube()
        
        comment <- tryCatch(
          
          get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),
          
          warning = function(w) {
            cat("[ExtractorYtbid][obtainDate][Warning] Date ytbid warning: ",
                      self$getId(), " ", paste(w),"\n")
          },
          
          error = function(e) {
            cat("[ExtractorYtbid][obtainDate][Error] Date ytbid error: ",
                      self$getId(), " ", paste(e),"\n")
          }
        )
      }
      
      Bdp4R[["private_fields"]][["connections"]]$addNumRequestToYoutube()
      
      if (!is.null(comment) && is.data.frame(comment)) {
        
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
            cat("[ExtractorYtbid][obtainDate][Warning] Date ytbid warning as.POSIXct: ",
                      self$getId(), " ", paste(w),"\n")
          },
          
          error = function(e) {
            cat("[ExtractorYtbid][obtainDate][Error] Date ytbid error as.POSIXct: ",
                      self$getId(), " ", paste(e),"\n")
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
        
        exportJSON <- rjson::toJSON(lista)
        
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
        
        cat(paste("[ExtractorYtbid][obtainDate][Error] exportJSON: ",
                  self$getId(), " " , paste(e), "\n"))
        
        lista <- list(source = "",
                      date = super$getDate())
        
        exportJSON <- rjson::toJSON(lista)
        
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
        
        dateYtbid <- ""
        sourceYtbid <- ""
        
        Bdp4R[["private_fields"]][["connections"]]$checkRequestToYoutube()
        
        comment <- tryCatch(
          
          get_comments(
            filter = c(comment_id = self$getId()),
            textFormat = "plainText"
          ),
          
          warning = function(w) {
            cat("[ExtractorYtbid][obtainSource][Warning] Source ytbid warning: ",
                      self$getId(), " ", paste(w),"\n")
          },
          
          error = function(e) {
            cat("[ExtractorYtbid][obtainSource][Error] Source ytbid error: ",
                      self$getId(), " ", paste(e),"\n")
          }
        )
      }
      
      Bdp4R[["private_fields"]][["connections"]]$addNumRequestToYoutube()
      
      if (!is.null(comment) && is.data.frame(comment)) {
        
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
            cat("[ExtractorYtbid][obtainSource][Warning] Date ytbid warning as.POSIXct: ",
                      self$getId(), " " , paste(w), "\n")
          },
          
          error = function(e) {
            cat("[ExtractorYtbid][obtainSource][Error] Date ytbid error as.POSIXct: ",
                      self$getId(), " " , paste(e), "\n")
          }
        )
        
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        
        dateYtbid <- as.character(format(StandardizedDate, formatDateGeneric))
        
      }
      
      sourceYtbid %>>%
          super$setSource()
      
      super$getSource() %>>%
        super$setData()
      
      lista <- list(source = super$getSource(),
                    date = dateYtbid)
      
      tryCatch({
        
        exportJSON <- rjson::toJSON(lista)
        
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
    
        cat("[ExtractorYtbid][obtainSource][Error] exportJSON: ",
                  self$getId(), " " , paste(e), "\n")
        
        lista <- list(source = "", date = dateYtbid)
        exportJSON <- rjson::toJSON(lista)
        
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
  
  private = list(
    id = ""
  )
)