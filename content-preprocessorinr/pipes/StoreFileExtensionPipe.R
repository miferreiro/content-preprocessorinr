#' @title Class to get the extension of a file
#' @description Class to get the extension of a file.
#' @docType class
#' @usage StoreFileExtensionPipe$new(propertyName = "extension",
#'                            alwaysBeforeDeps = list(""),
#'                            notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The pipe will invalidate the instance if it is not able to find the
#' extension from the path of the instance.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to obtain the extension of instance.
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
#' (Instance) Instance to preprocess.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{obtainExtension}}{
#' Getter of extension of the path.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{obtainExtension(path)}
#' }
#' \item{\emph{Value}}{
#'
#' Extension of the path.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{path}}{
#' (character) Path of the file to get the extension.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 tools pipeR
#' @export StoreFileExtensionPipe

StoreFileExtensionPipe <- R6Class(
    
  "StoreFileExtensionPipe",
  
  inherit = PipeGeneric,
    
  public = list(
    
    initialize = function(propertyName = "extension",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
 
      if (!"character" %in% class(propertyName)) {
        stop("[StoreFileExtensionPipe][initialize][Error] 
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StoreFileExtensionPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StoreFileExtensionPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance) {
             
      if (!"Instance" %in% class(instance)) {
        stop("[StoreFileExtensionPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      

      instance$addFlowPipes("StoreFileExtensionPipe")
      
      if (!instance$checkCompatibility("StoreFileExtensionPipe", self$getAlwaysBeforeDeps())) {
        stop("[StoreFileExtensionPipe][pipe][Error] Bad compatibility between Pipes.")
      }

      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getPath() %>>% 
        self$obtainExtension() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("extension") %in% "" ) {
        
        message <- c( "The file: " , instance$getPath() , " has not an extension")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[StoreFileExtensionPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
        
      }
            
      return(instance)
    },
        
    obtainExtension = function(path) {
    
      if (!"character" %in% class(path)) {
          stop("[StoreFileExtensionPipe][obtainExtension][Error] 
                  Checking the type of the variable: path ", 
                    class(path))
      }
        
      return(file_ext(path))
    }
  )
)
