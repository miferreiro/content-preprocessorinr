#' @title Class to get the target of the instance
#' @description This class allows searching in the path the target of the
#' instance.
#' @docType class
#' @usage TargetAssigningPipe$new(targets = list("ham","spam"),
#'                         targetsName = list("_ham_","_spam_"),
#'                         propertyName = "target",
#'                         alwaysBeforeDeps = list(),
#'                         notAfterDeps = list())
#' @param targets  (list) Name of the targets property.
#' @param targetsName  (list) The name of folders.
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The targets that are searched can be controlled through the
#' constructor of the class where targetsName will be the string that is
#' searched within the path and targets has the values that the property can
#' take.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to obtain the target.
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
#'
#' \item{\bold{getTarget}}{
#' Function to gets the target from a path.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getTarget(path)}
#' }
#' \item{\emph{Value}}{
#'
#' The target of the path.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{path}}{
#' (character) Path to analize.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{checkTarget}}{
#' Function to checks if the target is in the path.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{checkTarget(target, path)}
#' }
#' \item{\emph{Value}}{
#'
#' If the target is found, returns target, else returns "".
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{target}}{
#' (character) Target to find in the path.
#' }
#' \item{\strong{path}}{
#' (character) Path to analize.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getTargets}}{
#' Getter of targets.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getTargets()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of targets.
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{targets}}{
#'  (list) Name of the targets property.
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 pipeR
#' @importFrom stringi stri_detect_fixed
#' @export TargetAssigningPipe

TargetAssigningPipe <- R6Class(
    
  "TargetAssigningPipe",
  
  inherit = PipeGeneric,
    
  public = list(
        
    initialize = function(targets = list("ham","spam"),
                            targetsName = list("_ham_","_spam_"), 
                              propertyName = "target",  
                                alwaysBeforeDeps = list(), 
                                  notAfterDeps = list()) {
   
      if (!"list" %in% class(targets)) {
        stop("[TargetAssigningPipe][initialize][Error] 
                Checking the type of the variable: targets ", 
                  class(targets))
      }
      
      if (!"list" %in% class(targetsName)) {
        stop("[TargetAssigningPipe][initialize][Error] 
                Checking the type of the variable: targetsName ", 
                  class(targetsName))
      }
      
      if (!"character" %in% class(propertyName)) {
        stop("[TargetAssigningPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TargetAssigningPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TargetAssigningPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      private$targets <- targets
      names(private$targets) <- targetsName
       
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
    },    
       
    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[TargetAssigningPipe][pipe][Error] 
                 Checking the type of the variable: instance ", 
                   class(instance))
      }
     
      instance$addFlowPipes("TargetAssigningPipe")
      
      if (!instance$checkCompatibility("TargetAssigningPipe", self$getAlwaysBeforeDeps())) {
        stop("[TargetAssigningPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getPath() %>>% 
        self$getTarget() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("target") %in% "unrecognizable") {
        
        message <- c( "The file: " , instance$getPath() , " has a target unrecognizable")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[TargetAssigningPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
        
      return(instance)

    },
     
    getTarget = function(path) {
      
      if (!"character" %in% class(path)) {
        stop("[TargetAssigningPipe][getTarget][Error] 
                Checking the type of the variable: path ",
                  class(path))
      }
     
      for (target in names(self$getTargets())) {
        selectedTarget <- self$checkTarget(target,path)
       
        if (selectedTarget != "") {
          return(as.character(selectedTarget))
        }
      }
     
      return("unrecognizable")
    },
     
    checkTarget = function(target, path) {
          
      if (!"character" %in% class(target)) {
        stop("[TargetAssigningPipe][checkTarget][Error] 
                Checking the type of the variable: target ", 
                  class(target))
      }
     
      if (!"character" %in% class(path)) {
        stop("[TargetAssigningPipe][checkTarget][Error] 
                Checking the type of the variable: path ", 
                  class(path))
      }
     
      selectedTarget <- ""
     
      if (stri_detect_fixed(path,target)) {
        selectedTarget <- self$getTargets()[target]
      } 
     
      return(selectedTarget)
    },
     
    getTargets = function() {

      return(private$targets)
    }
  ),
  
  private = list(
    targets = list()
  )
)
