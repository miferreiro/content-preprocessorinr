#' @title Class to manage differents types of resources
#' @description Class to manage different types of resources.
#' @docType class
#' @usage ResourceHandler$new(propertyName,
#'                 alwaysBeforeDeps,
#'                 notAfterDeps)
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details It is a class that allows you to store the resources that are needed
#' in the pipes to avoid having to repeatedly read from the file. File resources
#' of type json are read and stored in memory.
#' @section Methods:
#' \itemize{
#' \item{\bold{isLoadResource}}{
#' From the resource path, it is checked if they have already been loaded. In
#' this case, the list of the requested resource is returned. Otherwise, the
#' resource variable is added to the list of resources, and the resource list is
#' returned. In the event that the resource file does not exist, NULL is returned.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{isLoadResource(pathResource)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{pathResource}}{
#' (character) Resource file path.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getResources}}{
#' Getter of resources variable.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getResources()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of resources variable.
#' }
#' }
#' }
#'
#' \item{\bold{setResources}}{
#' Setter of resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setResources(resources)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{resources}}{
#' (list) The new value of resources.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getNamesResources}}{
#' Getter of names of resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getNamesResources()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of names of resources.
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{resources}}{
#'  (list) Variable that stores the lists of the different types of resources.
#' }
#' }
#'
#' @import R6 rjson rlist
#' @export ResourceHandler

ResourceHandler <- R6Class(
  
  "ResourceHandler",
  
  public = list(
    
    initialize = function() {
      
    },
    
    isLoadResource = function(pathResource) {
      
      if (!"character" %in% class(pathResource)) {
        stop("[ResourceHandler][isLoadResource][Error] 
                Checking the type of the variable: pathResource ", 
                  class(pathResource));
      }
      
      if (pathResource %in% self$getNamesResources()) {
        
        return(self$getResources()[[pathResource]])
        
      } else {
        
        if (file.exists(pathResource)) {
          
          jsonData <- rjson::fromJSON(file = pathResource)
          self$setResources(list.append(self$getResources(), jsonData))
          names(private$resources)[length(self$getResources())] <- pathResource
          
          return(self$getResources()[[pathResource]])
          
        } else {
          return(NULL)
        }
      }
      return(listResource)
    },
    
    getResources = function() {

      return(private$resources)
    },
    
    setResources = function(resources) {

      private$resources <- resources
      
      return()
    },
    
    getNamesResources = function() {

      return(names(self$getResources()))
    }
  ),
  
  private = list(
    resources = list()
  )
)