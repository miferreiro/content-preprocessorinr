#' @title Class to find and/or remove the emoticon on the data
#' @description This class allows you to preprocess the data of an instance to
#' find the emoticons that are in it. Optionally, you can decide whether to
#' remove the data emoticons or not.
#' @docType class
#' @usage FindEmoticonPipe$new(propertyName = "Emoticon",
#'                      alwaysBeforeDeps = list(),
#'                      notAfterDeps = list("FindHashtagPipe"))
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The regular expression indicated in the \code{emoticonPattern}
#' variable is used to identify emoticons.
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
#' Function that preprocesses the instance to obtain/remove the emoticons.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, removeEmoticon = TRUE)}
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
#' \item{\strong{removeEmoticon}}{
#' (logical) Indicates if the emoticons are replaced.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findEmoticon}}{
#' Function that find the emoticons in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findEmoticon(data)}
#' }
#' \item{\emph{Value}}{
#'
#' List with emoticons found.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the emoticons are searched.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeEmoticon}}{
#' Function that removes the emoticons in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{removeEmoticon(data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with emoticons removed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which emoticons will be removed.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{emoticonPattern}}{
#'  (character) Regular expression to detect emoticons.
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
#' @export FindEmoticonPipe

FindEmoticonPipe <- R6Class(
    
  "FindEmoticonPipe",
    
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "Emoticon",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list("FindHashtagPipe")) {

      if (!"character" %in% class(propertyName)) {
        stop("[FindEmoticonPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindEmoticonPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindEmoticonPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
    }, 

    emoticonPattern = '(\\:\\w+\\:|\\<[\\/\\\\]?3|[\\(\\)\\\\\\D|\\*\\$][\\-\\^]?[\\:\\;\\=]|[\\:\\;\\=B8][\\-\\^]?[3DOPp\\@\\$\\*\\\\\\)\\(\\/\\|])(?=\\s|[\\!\\.\\?\\:\\w<>]|$)',
        
    pipe = function(instance, removeEmoticon = TRUE){
        
      if (!"Instance" %in% class(instance)) {
        stop("[FindEmoticonPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(removeEmoticon)) {
        stop("[FindEmoticonPipe][pipe][Error]
                Checking the type of the variable: removeEmoticon ", 
                  class(removeEmoticon))
      }

      instance$addFlowPipes("FindEmoticonPipe")
      
      if (!instance$checkCompatibility("FindEmojiInStringBufferPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindEmoticonPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getData() %>>%
        self$findEmoticon() %>>%
          unique() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}
      
      if (removeEmoticon) {
          instance$getData()  %>>%
            self$removeEmoticon() %>>%
              trim() %>>%
                instance$setData()
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " hsas data empty on pipe Emoticon")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindEmoticonPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findEmoticon = function(data){

      if (!"character" %in% class(data)) {                    
        stop("[FindEmoticonPipe][findEmoticon][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      return(str_match_all(data,
                           regex(self$emoticonPattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2])
    },
        
    removeEmoticon = function(data){
    
      if (!"character" %in% class(data)) {
        stop("[FindEmoticonPipe][removeEmoticon][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(str_replace_all(data,
                             regex(self$emoticonPattern,
                                   ignore_case = TRUE,
                                   multiline = TRUE), " "))
    }
  )
)