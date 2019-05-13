#' @title Class to find and/or remove the users on the data
#' @description This class allows you to preprocess the data of an instance to
#' find the user names that are in it. Optionally, you can decide whether to
#' remove the data user names or not.
#' @docType class
#' @usage FindUserNamePipe$new(propertyName = "userName",
#'                      alwaysBeforeDeps = list(),
#'                      notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The regular expressions indicated in the \code{userPattern}
#' variable are used to identify user names.
#'
#' The pipe will invalidate the instance in the moment that the resulting data is
#' empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to obtain/remove the name users.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, removeUser = TRUE)}
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
#' \item{\strong{removeUser}}{
#' (logical) Indicates if the users are removed.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findUserName}}{
#' Function that find the users in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findHashtag(data)}
#' }
#' \item{\emph{Value}}{
#'
#' List with users found.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the user names are searched.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeUserName}}{
#' Function that removes the users in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{removeUserName(data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with users removed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which name users will be removed.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{userPattern}}{
#'  (character) Regular expression to detect users.
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 rlist pipeR
#' @importFrom textutils trim
#' @importFrom rex regex
#' @importFrom stringr str_match_all
#' @importFrom stringr str_replace_all
#' @export FindUserNamePipe

FindUserNamePipe <- R6Class(
    
  "FindUserNamePipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "userName",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
        
      if (!"character" %in% class(propertyName)) {
        stop("[FindUserNamePipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindUserNamePipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindUserNamePipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    }, 
    
    userPattern = "(?:\\s|^|[\"><¡¿?!;:,.'-])(@[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",
        
    pipe = function(instance, removeUser = TRUE){
  
      if (!"Instance" %in% class(instance)) {
        stop("[FindUserNamePipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(removeUser)) {
          stop("[FindUserNamePipe][pipe][Error]
                  Checking the type of the variable: removeUser ", 
                    class(removeUser))
      }
              
      instance$addFlowPipes("FindUserNamePipe")
      
      if (!instance$checkCompatibility("FindUserNamePipe", self$getAlwaysBeforeDeps())) {
        stop("[FindUserNamePipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getData() %>>% 
        self$findUserName() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}
      
      if (removeUser) {
        instance$getData()  %>>%
          self$removeUserName() %>>%
            trim() %>>%
              instance$setData()
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe UserName")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindUserNamePipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findUserName = function(data) {
    
      if (!"character" %in% class(data)) {
        stop("[FindUserNamePipe][findUserName][Error] 
                Checking the type of the variable: data ", 
             class(data))
      }
      
      return(str_match_all(data,
                           regex(self$userPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },
    
    removeUserName = function(data) {
          
      if (!"character" %in% class(data)) {
        stop("[FindUserNamePipe][removeUserName][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_replace_all(data,
                             regex(self$userPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)