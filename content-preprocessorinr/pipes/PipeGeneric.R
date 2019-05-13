#' @title Super class that handles the general functionalities of the management
#' of the pipes
#' @description Super class that handles the general functionalities of the
#' pipes.
#' @docType class
#' @usage PipeGeneric$new(propertyName,
#'                 alwaysBeforeDeps,
#'                 notAfterDeps)
#' @param propertyName  (character) Name of the property.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details Building...
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Abtract method to preprocess the intance.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{instance}}{
#' (Instance) Instance to preprocess.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPropertyName}}{
#' Getter of name of property.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPropertyName()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of name of property.
#' }
#' }
#' }
#'
#' \item{\bold{getAlwaysBeforeDeps}}{
#' Getter of the dependences always before.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getAlwaysBeforeDeps()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of dependences always before.
#' }
#' }
#' }
#'
#' \item{\bold{getNotAfterDeps}}{
#' Getter of the dependences not after.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getNotAfterDeps()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of dependences not after.
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{propertyName}}{
#'  (character) The name of property.
#' }
#' \item{\bold{alwaysBeforeDeps}}{
#'  (list) Dependencies of the type alwaysBefore. These dependences indicate
#'  what pipes must be executed before the current one.
#' }
#' \item{\bold{notAfterDeps}}{
#'  (list) Dependencies of the type notAfter. These dependences indicate what
#'  pipes must not be executed after the current one.
#' }
#' }
#'
#' @seealso \code{\link{Instance}}
#'
#' @import R6
#' @export PipeGeneric

PipeGeneric <- R6Class(
  
  "PipeGeneric",
  
  public = list(
        
    initialize = function(propertyName, alwaysBeforeDeps, notAfterDeps) {
 
      if (!"character" %in% class(propertyName)) {
        stop("[PipeGeneric][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[PipeGeneric][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[PipeGeneric][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      private$propertyName <- propertyName
      private$alwaysBeforeDeps <- alwaysBeforeDeps
      private$notAfterDeps <- notAfterDeps

    },    
    
    pipe = function(instance) {

      stop("I'm an abstract interface method")
    },
    
    getPropertyName = function() {

      return(private$propertyName)
    },
    
    getAlwaysBeforeDeps = function() {

      return(private$alwaysBeforeDeps)
    },
    
    getNotAfterDeps = function() {

      return(private$notAfterDeps)
    }
  ),
  
  private = list(
    propertyName = "",
    alwaysBeforeDeps = list() ,
    notAfterDeps = list()
  )
)
