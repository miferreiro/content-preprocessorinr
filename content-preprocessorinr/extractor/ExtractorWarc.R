#' @title Class to handle warc files
#' @description It is a class that inherits from the \code{Instance} class and
#' implements the functions of extracting the text and the date of an warc-type
#' file.
#' @docType class
#' @usage ExtractorWarc$new(path)
#' @param path  (character) Path of the warc-type file.
#' @details The read_warc function of the jwart package was overwritten because
#' it returned the hours wrong.
#'
#' The jwart package makes calls to Java so it is necessary to have rJava
#' installed.
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainDate}}{
#' Function that obtains the date of the warc file. Finds the warcinfo type records
#' in which the date appearss and standardizes it with the format:
#' "\%a \%b \%d \%H:\%M:\%S \%Z \%Y" (Example: "Thu May 02 06:52:36 UTC 2013").
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainDate()}
#' }
#' }
#' }
#'
#' \item{\bold{obtainSource}}{
#' Function that obtains the source of the warc file. The list of records that
#' contain information are obtained, which they are resource and response. Then
#' they are traversed and the charset of that record is obtained. If that charset
#' matches the one obtained from guess_encoding, payload_content is used to get
#' the contents of the record. If it does not match, the content is obtained,
#' converting the content in bytes to string. This is done because there are
#' coding problems in cases that the charset that is detected is different from
#' the one that is really. In addition it initializes the data with the initial
#' source.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainSource()}
#' }
#' }
#' }
#'
#' }
#'
#' @seealso \code{\link{Instance}}
#'
#' @import R6 pipeR jwatr rlist readr rJava
#' @importFrom stringr str_match
#' @export ExtractorWarc
# jwatjars

ExtractorWarc <- R6Class(
  
  classname = "ExtractorWarc",
  
  inherit = Instance,
  
  public = list(
    
    initialize = function(path) {

      if (!"character" %in% class(path)) {
        stop("[ExtractorWarc][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }
      path %>>%
        super$initialize()
    },
    
    obtainDate = function() {

      xdfDate <- read_warc(super$getPath(), 
                           warc_types = c( "warcinfo"), 
                           include_payload = FALSE)

      for (i in 1:dim(xdfDate)[1]) {
          date <- xdfDate$date
          StandardizedDate <- as.POSIXct(date)
          formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
          format(StandardizedDate, formatDateGeneric) %>>%
            super$setDate()

      }
      
      return()
    },
    
    obtainSource = function() {

      rawData <- list()
      
      xdf <- read_warc(super$getPath(), 
                       warc_types = c( "response", "resource"), 
                       include_payload = TRUE)
      
      xdfHtmlPlain <- dplyr::filter(xdf, 
                                    grepl("(html|plain)", 
                                          http_protocol_content_type))

      numRecords <- dim(xdfHtmlPlain)[1]

      if (numRecords != 0 ) {
      
        for (i in 1:numRecords) {
  
          if (grepl("response",xdfHtmlPlain$warc_type[i])) {
            
            charset <- toupper(str_match(pattern = "\\bcharset=\\s*\"?([^\\s;\"]*)", 
                                         xdfHtmlPlain$http_protocol_content_type[[i]])[2])
  
            if (!is.na(charset) && 
                  guess_encoding(super$getPath())[[1]][[1]] == charset) {
              
              payload <-
                payload_content(
                  url = xdfHtmlPlain$target_uri[i],
                  ctype = xdfHtmlPlain$http_protocol_content_type[i],
                  headers = xdfHtmlPlain$http_raw_headers[[i]],
                  payload = xdfHtmlPlain$payload[[i]],
                  enconding = charset,
                  as = "text"
                )
    
               rawData <- list.append(rawData, payload)
               
            } else {
              
              rawData <- list.append(rawData,rawToChar(xdfHtmlPlain$payload[[1]]))
              
            }
              
          } else {
            
            if (grepl("resource", xdfHtmlPlain$warc_type[i])) {
              
              charset <- toupper(str_match(pattern = "\\bcharset=\\s*\"?([^\\s;\"]*)",
                                           xdfHtmlPlain$http_protocol_content_type[[i]])[2])
              
              if (!is.na(charset) && 
                    guess_encoding(super$getPath())[[1]][[1]] == charset) {
                
                payload <-
                  payload_content(
                    url = xdfHtmlPlain$target_uri[i],
                    ctype = xdfHtmlPlain$http_protocol_content_type[i],
                    headers = xdfHtmlPlain$http_raw_headers[[i]],
                    payload = xdfHtmlPlain$payload[[i]],
                    enconding = charset,
                    as = "text"
                  )
                
                rawData <- list.append(rawData, payload)
                
              } else {
                
                rawData <- list.append(rawData, rawToChar(xdfHtmlPlain$payload[[1]]))  
                
              }
            }
          }
        }
      }
      # In some cases the library returns an array of strings. 
      # Then everything is converted to a single string
      rawData <- paste(rawData, collapse = " ")
      
      rawData %>>%
        super$setSource()
      
      super$getSource() %>>%
        super$setData()
      
      return()
    }#End of the function obtainSource
  )
)