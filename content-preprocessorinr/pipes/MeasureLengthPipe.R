#' @title Class to obtain the length of the data of an instance
#' @description This class allows you to know the length of the data of an
#' instance.
#' @docType class
#' @usage MeasureLengthPipe$new(propertyName = "length",
#'                       alwaysBeforeDeps = list(),
#'                       notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to obtain the length of data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, propertyName = super$getPropertyName(),
#' nchar_conf = TRUE)}
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
#' \item{\strong{propertyName}}{
#' (character) The name of the property that will be obtained in the pipe.
#' }
#' \item{\strong{nchar_conf}}{
#' (logical)
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getLength}}{
#' Function that obtain the length of the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getLength(data, nchar_conf = TRUE)}
#' }
#' \item{\emph{Value}}{
#'
#' The instance with the modifications that have occurred in the pipe.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text to preproccess.
#' }
#' \item{\strong{nchar_conf}}{
#' (logical)
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 pipeR
#' @importFrom utils object.size
#' @export MeasureLengthPipe

MeasureLengthPipe <- R6Class(
    
  "MeasureLengthPipe",
  
  inherit = PipeGeneric,
    
  public = list(
        
    initialize = function(propertyName = "length",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
     
      if (!"character" %in% class(propertyName)) {
        stop("[MeasureLengthPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[MeasureLengthPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[MeasureLengthPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance,
                    propertyName = super$getPropertyName(),
                    nchar_conf = TRUE) {
      
        if (!"Instance" %in% class(instance)) {
            stop("[MeasureLengthPipe][pipe][Error] 
                    Checking the type of the variable: instance ", 
                      class(instance))
        }
        
        if (!"character" %in% class(propertyName)) {
            stop("[MeasureLengthPipe][pipe][Error] 
                    Checking the type of the variable: propertyName ", 
                      class(propertyName))
        }

        if (!"logical" %in% class(nchar_conf)) {
            stop("[MeasureLengthPipe][pipe][Error] 
                    Checking the type of the variable: nchar_conf ", 
                      class(nchar_conf))
        }
        
        instance$addFlowPipes("MeasureLengthPipe")
        
        if (!instance$checkCompatibility("MeasureLengthPipe", self$getAlwaysBeforeDeps())) {
          stop("[MeasureLengthPipe][pipe][Error] Bad compatibility between Pipes.")
        }
        
        instance$addBanPipes(unlist(super$getNotAfterDeps()))
        
        instance$getData() %>>% 
          {self$getLength(.,nchar_conf)} %>>%
            {instance$addProperties(.,propertyName)}
        
        return(instance);
    },
    
    getLength = function(data, nchar_conf = TRUE) {

      if (!"character" %in% class(data)) {
        stop("[MeasureLengthPipe][getLength][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      if (!"logical" %in% class(nchar_conf)) {
        stop("[MeasureLengthPipe][getLength][Error] 
                Checking the type of the variable: nchar_conf ", 
                  class(nchar_conf))
      }
        
      return(ifelse(nchar_conf, nchar(data), object.size(data)))
    }
  )
)
