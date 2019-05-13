#' @title Class to obtain the source of an instance
#' @description The method of obtaining source is called which implement the
#' subclasses of the superclass \code{Instance}.
#' @docType class
#' @usage File2Pipe$new(propertyName = "source",
#'               alwaysBeforeDeps = list("TargetAssigningPipe"),
#'               notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details In the case that the source obtained is empty or is not utf-8,
#' the instance is invalidated.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to obtain the source.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance)}
#' }
#' \item{\emph{Value}}{
#'
#' The instance with the modifications that have occurred in the pipe.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{instance}}{
#' (Instance) Instance to preproccess.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6
#' @export File2Pipe

File2Pipe <- R6Class(
    
  "File2Pipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "source",  
                          alwaysBeforeDeps = list("TargetAssigningPipe"), 
                          notAfterDeps = list()) {
          
      if (!"character" %in% class(propertyName)) {
        stop("[File2Pipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[File2Pipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[File2Pipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance){
     
      if (!"Instance" %in% class(instance)) {
        stop("[File2Pipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      instance$addFlowPipes("File2Pipe")
      
      if (!instance$checkCompatibility("File2Pipe", self$getAlwaysBeforeDeps())) {
        stop("[File2Pipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$obtainSource()
        
      if (is.na(instance$getSource()) || all(instance$getSource() == "") || is.null(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " has source empty")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[File2Pipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      if (!validUTF8(instance$getSource())) {
        message <- c( "The file: " , instance$getPath() , " is not utf8")
        
        instance$addProperties(message, "reasonToInvalidate")  
        
        cat("[File2Pipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
        
      return(instance)
    }
  )
)
