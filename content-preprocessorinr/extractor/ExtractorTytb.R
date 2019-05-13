#' @title Class to handle tytb files
#' @description It is a class that inherits from the \code{Instance} class and
#' implements the functions of extracting the text and the date of an Tytb-type
#' file.
#' @docType class
#' @usage ExtractorTytb$new(path)
#' @param path  (character) Path of the tytb-type file.
#' @details Due to the fact that the creation date of the message can not be
#' extracted from the text of an comment from youtube, the date will be
#' initialized to empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainDate}}{
#' Function that obtains the date of the tytb file.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainDate()}
#' }
#' }
#' }
#'
#' \item{\bold{obtainSource}}{
#' Function that obtains the source of the tytb file. Reads the file indicated in
#' the path. In addition, it initializes the data with the initial source.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainSource()}
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{Instance}}
#'
#' @import R6 pipeR readr
#' @export ExtractorTytb

ExtractorTytb <- R6Class(
  
  classname = "ExtractorTytb",
  
  inherit = Instance,
  
  public = list(
    
    initialize = function(path) {

      if (!"character" %in% class(path)) {
        stop("[ExtractorTytb][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }
      path %>>%
        super$initialize()
      
    },
    
    obtainDate = function() {

      "" %>>%
        super$setDate()
      
      return()
    },
    
    obtainSource = function() {

      super$getPath() %>>%
        read_file() %>>%
          super$setSource()
      
      super$getSource() %>>%
        super$setData()
      
      return()
    }
  )
)