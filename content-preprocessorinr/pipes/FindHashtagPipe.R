#' @title Class to find and/or remove the hashtags on the data
#' @description This class allows you to preprocess the data of an instance to
#' find the hashtags that are in it. Optionally, you can decide whether to
#' remove the data hashtags or not.
#' @docType class
#' @usage FindHashtagPipe$new(propertyName = "hashtag",
#'                      alwaysBeforeDeps = list(),
#'                      notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The regular expression indicated in the \code{hashtagPattern}
#' variable is used to identify hashtags.
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
#' Function that preprocesses the instance to obtain/remove the hashtags.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, removeHashtag = TRUE)}
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
#' \item{\strong{removeHashtag}}{
#' (logical) Indicates if the hashstags are replaced.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findHashtag}}{
#' Function that find the hashtags in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findHashtag(data)}
#' }
#' \item{\emph{Value}}{
#'
#' List with hashtags found.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the hashtags are searched.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeHashtag}}{
#' Function that removes the hashtags in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{removeHashtag(data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with hashtags removed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which hashtags will be removed.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{hashtagPattern}}{
#'  (character) Regular expression to detect hashtags.
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
#' @export FindHashtagPipe

FindHashtagPipe <- R6Class(
        
  "FindHashtagPipe",
        
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "hashtag",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[FindHashtagPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindHashtagPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindHashtagPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
            
    hashtagPattern = "(?:\\s|^|[\"><¡¿?!;:,.'-])(#[^[:cntrl:][:space:]!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.'>-]?(?=(?:\\s|$|>))",          
    
    pipe = function(instance, removeHashtag = TRUE){
               
      if (!"Instance" %in% class(instance)) {
        stop("[FindHashtagPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
          
      if (!"logical" %in% class(removeHashtag)) {
        stop("[FindHashtagPipe][pipe][Error]
                Checking the type of the variable: removeHashtag ", 
                  class(removeHashtag))
      }
      
      instance$addFlowPipes("FindHashtagPipe")
      
      if (!instance$checkCompatibility("FindHashtagPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindHashtagPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getData() %>>% 
        self$findHashtag() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}
      
      if (removeHashtag) {
          instance$getData()  %>>%
            self$removeHashtag() %>>%
              trim() %>>%
                {instance$setData(.)}
      }    
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Hashtag")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindHashtagPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance);
    },
    
    findHashtag = function(data){
   
      if (!"character" %in% class(data)) {
        stop("[FindHashtagPipe][findHashtag][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_match_all(data,
                           regex(self$hashtagPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },
    
    removeHashtag = function(data){
            
      if (!"character" %in% class(data)) {
        stop("[FindHashtagPipe][removeHashtag][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(str_replace_all(data,
                             regex(self$hashtagPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)