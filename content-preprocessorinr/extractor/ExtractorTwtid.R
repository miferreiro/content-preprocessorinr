#' @title Class to handle twtid files
#' @description It is a class that inherits from the \code{Instance} class and
#' implements the functions of extracting the text and the date of an twtid-type
#' file.
#' @docType class
#' @usage ExtractorTwtid$new(path)
#' @param path  (character) Path of the twtid-type file.
#' @details The connection to twitter is handled through the \code{Connections}
#' class, which needs a configuration file with the necessary keys to make
#' requests to the twitter API.
#'
#' This class stores in the folder testFiles/cache/hsspam14/tweets the
#' tweets processed so far, this allowing you to save twitter queries. The text
#' fields, the date and the language of the tweet are stored.
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainId}}{
#' Function that obtains the id of the twtid. Reads the id of the file indicated
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
#' Getter of tweet id.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getId()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of tweet id.
#' }
#' }
#' }
#'
#' \item{\bold{obtainDate}}{
#' Function that obtains the date of the twtid id. Check if the tweet has
#' previously been cached. In this case, the file is read in json format and the
#' date is stored. Otherwise, the request is made on twitter. The date is then
#' formatted to "\%a \%b \%d \%H:\%M:\%S \%Z \%Y"
#' (Example: "Thu May 02 06:52:36 UTC 2013").
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
#'  (character) Id of tweet.
#' }
#' }
#'
#' @seealso \code{\link{Instance}}
#'
#' @import R6 pipeR rtweet rjson
#' @export ExtractorTwtid

ExtractorTwtid <- R6Class(
  
  classname = "ExtractorTwtid",
  
  inherit = Instance,
  
  public = list(
    
    initialize = function(path) {

      path %>>%
        super$initialize()
      
      self$obtainId()
      #Singleton
      Bdp4R[["private_fields"]][["connections"]]$startConnectionWithTwitter()
      
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
        
        Bdp4R[["private_fields"]][["connections"]]$checkRequestToTwitter()

        lookup <- tryCatch(
          
          self$getId() %>>%
            as.character() %>>%
              rtweet::lookup_tweets(.,p = Bdp4R[["private_fields"]][["connections"]]$getTwitterToken()),
          
          warning = function(w) {
            cat(paste("[ExtractorTwtid][obtainDate][Warning] Date twtid warning: ",
                      self$getId(), " ", paste(w)),"\n")
          },
          
          error = function(e) {
            cat(paste("[ExtractorTwtid][obtainDate][Error] Date twtid error: ",
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
        
        Bdp4R[["private_fields"]][["connections"]]$checkRequestToTwitter()
        
        lookup <- tryCatch(
          self$getId() %>>%
            as.character() %>>%
              rtweet::lookup_tweets(.,p = Bdp4R[["private_fields"]][["connections"]]$getTwitterToken()),
          
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