#' @title Class to obtain the date
#' @description The method of obtaining date is called which implement the
#' subclasses of the superclass Instance.
#' @docType class
#' @usage GuessDatePipe$new(propertyName = "date",
#'                   alwaysBeforeDeps = list("TargetAssigningPipe"),
#'                   notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @section Inherit:
#' This class inherit from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to obtain the date.
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
#' @export GuessDatePipe

GuessDatePipe <- R6Class(
    
  "GuessDatePipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "date",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
    
      if (!"character" %in% class(propertyName)) {
        stop("[GuessDatePipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[GuessDatePipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[GuessDatePipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance) {
    
      if (!"Instance" %in% class(instance)) {
        stop("[GuessDatePipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      instance$addFlowPipes("GuessDatePipe")
      
      if (!instance$checkCompatibility("GuessDatePipe", self$getAlwaysBeforeDeps())) {
        stop("[GuessDatePipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$obtainDate()
      
      return(instance)
    }
  )
)
