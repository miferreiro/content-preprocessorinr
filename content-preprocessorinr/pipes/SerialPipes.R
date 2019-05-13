#' @title Class that implements the flow of pipes
#' @description This class inherits from the TypePipe class, which has the
#' pipeAll method that has a default implementation.
#' @docType class
#' @usage SerialPipes$new()
#' @details This class uses the default implementation provided by its parent
#' class and manages unexpected errors that may appear in the execution of the
#' pipes.
#' @section Inherit:
#' This class inherits from \code{\link{TypePipe}} and implements the
#' \code{pipeAll} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipeAll}}{
#' Function that implements the pipeAll function of the super class. In this
#' case, the pipeAll function is called so that the default pipe stream is
#' executed
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipeAll(instance)}
#' }
#' \item{\emph{Value}}{
#'
#' The preprocessed instance.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{instance}}{
#' (Instance) The instance that is going to be processed.
#' }
#' }
#' }
#' }
#' }
#'
#' }
#'
#' @seealso \code{\link{TypePipe}}, \code{\link{Instance}}
#' @import R6
#' @export SerialPipes

SerialPipes <- R6Class(
  
  "SerialPipes",
  
  inherit = TypePipe,
  
  public = list(
    
    initialize = function() {
      
    },
    
    pipeAll = function(instance) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[SerialPipes][pipeAll][Error] 
                Checking the type of the variable: instance ", 
                  class(instance));
      }
      
      cat("[SerialPipes][pipeAll][Info] ", instance$getPath(), "\n")
      
      tryCatch(
        instance <- super$pipeAll(instance)
      ,
        error = function(e) {
          cat("[SerialPipes][pipeAll][Error]", instance$getPath()," :", paste(e), "\n")
          instance$invalidate()
        }
      )
    
      return(instance)
    }
  )
)