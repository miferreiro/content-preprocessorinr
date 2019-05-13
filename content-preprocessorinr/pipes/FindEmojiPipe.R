#' @title Class to find and/or replace the emoji on the data of an instance
#' @description This class allows you to preprocess the data of an instance to
#' find the emojis that are in it. Optionally, you can decide whether to
#' replace the data emojis or not.
#' @docType class
#' @usage FindEmojiPipe$new(propertyName = "Emojis",
#'                   alwaysBeforeDeps = list(),
#'                   notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The emoji list that is used is the one provided by the variable
#' \code{emojis} of the rtweet package.
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
#' Function that preprocesses the instance to obtain/replace the abbreviations.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, replaceEmoji = TRUE)}
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
#' \item{\strong{replaceEmoji}}{
#' (logical) Indicate if the emojis are replaced.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findEmoji}}{
#' Function that checks if the emoji is in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findEmoji(data, emoji)}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE or FALSE depending on whether the emoji is on the data.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the emoji is searched.
#' }
#' \item{\strong{emoji}}{
#' (character) Indicates the emoji to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{replaceEmoji}}{
#' Function that replaces the emoji in the data for the extendedEmoji.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{replaceEmoji(emoji, extendedEmoji, data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with emoji replaced.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{emoji}}{
#' (character) Indicates the emoji to remove.
#' }
#' \item{\strong{extendedEmoji}}{
#' (character) Indicates the string to replace for the emoji found.
#' }
#' \item{\strong{data}}{
#' (character) Text in which emojis will be replaced.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 rlist pipeR rtweet
#' @importFrom textutils trim
#' @importFrom rex regex
#' @importFrom rex escape
#' @export FindEmojiPipe

FindEmojiPipe <- R6Class(
    
  "FindEmojiPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "Emojis",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[FindEmojiPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindEmojiPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindEmojiPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
    }, 
    
    pipe = function(instance, replaceEmoji = TRUE) {
            
      if (!"Instance" %in% class(instance)) {
        stop("[FindEmojiPipe][pipe][Error]
                Checking the type of the variable: instance ",
                  class(instance))
      }
        
      if (!"logical" %in% class(replaceEmoji)) {
        stop("[FindEmojiPipe][pipe][Error]
                Checking the type of the variable: replaceEmoji ", 
                  class(replaceEmoji))
      }
      
      instance$addFlowPipes("FindEmojiPipe")
      
      if (!instance$checkCompatibility("FindEmojiPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindEmojiPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      emojisLocated <- list()
      
      emojisList <- as.list(rtweet::emojis[2][[1]])
      names(emojisList) <- as.list(rtweet::emojis[[1]][])

      for (emoji in names(emojisList)) {

        if (self$findEmoji(instance$getData(), emoji)) {  
          
          emojisLocated <- list.append(emojisLocated, emoji) 
        }
        
        if (replaceEmoji && emoji %in% emojisLocated) {
          
          instance$getData() %>>%
            {self$replaceEmoji(emoji, emojisList[[emoji]], .)} %>>%
              trim() %>>%
                instance$setData()
        }
      }     
      
      instance$addProperties(paste(emojisLocated),super$getPropertyName()) 

      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Emoji")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[FindEmojiPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      return(instance)
    },
        
    findEmoji = function(data, emoji) {
    
      if (!"character" %in% class(data)) {                    
        stop("[FindEmojiPipe][findEmoji][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(emoji)) {                    
        stop("[FindEmojiPipe][findEmoji][Error] 
                Checking the type of the variable: emoji ", 
                  class(emoji))
      }

      return(grepl(pattern = rex::escape(emoji), x = data, fixed = T, useBytes = T))
      
    },    
    
    replaceEmoji = function(emoji, extendedEmoji, data ) {
      
      if (!"character" %in% class(data)) {
        stop("[FindEmojiPipe][replaceEmoji][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(emoji)) {                    
        stop("[FindEmojiPipe][replaceEmoji][Error] 
                Checking the type of the variable: emoji ", 
                  class(emoji))
      }
      
      if (!"character" %in% class(extendedEmoji)) {
        stop("[FindEmojiPipe][replaceEmoji][Error] 
                Checking the type of the variable: extendedEmoji ", 
                  class(extendedEmoji))
      }     
      
      return(gsub(rex::escape(emoji), 
                  paste(" ", extendedEmoji, " ", sep = ""), data, perl = T))
    }
  )
)
