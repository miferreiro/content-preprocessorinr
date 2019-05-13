#' @title Class to handle eml files
#' @description It is a class that inherits from the \code{Instance} class and
#' implements the functions of extracting the text and the date of an eml-type
#' file.
#' @docType class
#' @usage ExtractorSms$new(path, pathKeys = "config/configurations.ini")
#' @param path  (character) Path of the eml-type file.
#' @param pathKeys  (character) Path of the .ini file that contains the
#' configurations to read the eml files.
#' @details The way to indicate which part to choose in the email is through an
#' .ini file which contains the following structure (being xxxx, text / plain or
#' text / html):
#'
#' [eml]
#'
#' PartSelectedOnMPAlternative = xxxx
#'
#' To be able to use this class it is necessary to have python installed.
#'
#'
#' @section Inherit:
#' This class inherits from \code{\link{Instance}} and implements the
#' \code{obtainSource} and \code{obtainDate} abstracts functions.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainDate}}{
#' Function that obtain the date of the eml file. Call the function read_emails
#' and obtain the date of the file indicated in the path and then transforms it
#' into the generic date format, that is "\%a \%b \%d \%H:\%M:\%S \%Z \%Y"
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
#' Function that obtains the source of the eml file. Calls the function read_emails
#' and obtains the source of the file indicated in the path. In addition, it
#' initializes the data with the initial source.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainSource()}
#' }
#' }
#' }
#'
#' \item{\bold{getPartSelectedOnMPAlternative}}{
#' Getter of PartSelectedOnMPAlternative variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPartSelectedOnMPAlternative()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of PartSelectedOnMPAlternative variable.
#' }
#' }
#' }
#'
#' \item{\bold{setPartSelectedOnMPAlternative}}{
#' Setter of PartSelectedOnMPAlternative variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setPartSelectedOnMPAlternative(PartSelectedOnMPAlternative)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{PartSelectedOnMPAlternative}}{
#' (character) The new value of PartSelectedOnMPAlternative variable.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{PartSelectedOnMPAlternative}}{
#'  (character) Configuration to read the eml files. Indicates whether the
#'  text/plain part or the text/html part is read.
#' }
#' }
#'
#' @seealso \code{\link{Instance}}
#' @import R6 pipeR
#' @export ExtractorEml

ExtractorEml <- R6Class(
  
  classname = "ExtractorEml",
  
  inherit = Instance,
  
  public = list(
    
    initialize = function(path, 
                          pathKeys = "content-preprocessorinr/config/configurations.ini") {

      if (!"character" %in% class(path)) {
        stop("[ExtractorEml][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }
      
      if (!"character" %in% class(pathKeys)) {
        stop("[ExtractorEml][initialize][Error]
                Checking the type of the variable: pathKeys ",
                  class(pathKeys))
      }
      
      if (!"ini" %in% file_ext(pathKeys)) {
        stop("[ExtractorEml][initialize][Error]
                Checking the extension of the file: pathKeys ",
                  file_ext(pathKeys))
      }
      
      path %>>%
        super$initialize()
      
      read.ini(pathKeys)$eml$PartSelectedOnMPAlternative %>>%
        self$setPartSelectedOnMPAlternative()
    },
    
    obtainDate = function() {

      dateEml <- tryCatch(
        
        read_emails(super$getPath(),self$getPartSelectedOnMPAlternative())@date,
        
        warning = function(w) {
          cat(paste("[ExtractorEml][obtainDate][Warning] Date eml warning ", 
                         super$getPath()," ", paste(w), "\n"))
        },
        
        error = function(e) {
          cat(paste("[ExtractorEml][obtainDate][Error] Date eml error ", 
                         super$getPath()," ", paste(e), "\n"))
        }
      )
       
      tryCatch({
        formatDateEml <- "%a, %d %b %Y %H:%M:%S %z"
        StandardizedDate <- as.POSIXct(dateEml[[1]], format = formatDateEml)
        formatDateGeneric <- "%a %b %d %H:%M:%S %Z %Y"
        format(StandardizedDate, formatDateGeneric) %>>%
          super$setDate()
        },
        error = function(e) {
          cat(paste("[ExtractorEml][obtainDate][Error] Date eml error in 
                      standardized proccess", super$getPath(), " ", paste(e), "\n"))
        }
      )
       
      return()
    },
    
    obtainSource = function() {

      private$source <- tryCatch(
        
        paste(read_emails(super$getPath(), 
                          self$getPartSelectedOnMPAlternative())@message,
              collapse = " "),
        
        warning = function(w) {
          cat(paste("[ExtractorEml][obtainSource][Warning] Source eml warning ", 
                    super$getPath()," ", paste(w), "\n"))
        },
        
        error = function(e) {
          cat(paste("[ExtractorEml][obtainSource][Error] Source eml error ", 
                    super$getPath()," ", paste(e), "\n"))
        }
      )
      
      super$getSource() %>>%
        super$setData()
      
      return()
    },
    
    getPartSelectedOnMPAlternative = function() {

      return(private$PartSelectedOnMPAlternative)
    },
    
    setPartSelectedOnMPAlternative = function(PartSelectedOnMPAlternative) {
    
      private$PartSelectedOnMPAlternative <- PartSelectedOnMPAlternative
      
      return()
    }   
    
  ),

  private = list(
    PartSelectedOnMPAlternative = "text/plain"
  )
)
