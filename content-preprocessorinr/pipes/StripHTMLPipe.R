#' @title Class to remove html tags
#' @description Class to remove html tags.
#' @docType class
#' @usage StripHTMLPipe$new(propertyName = "",
#'                   alwaysBeforeDeps = list(),
#'                   notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The pipe will invalidate the instance in the moment that the
#' resulting data is empty.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to remove html tags.
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
#' \item{\bold{getDataWithOutHtml}}{
#' Function to gets the data without html tags
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getDataWithOutHtml(data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data witouth html tags
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text to removes html tags.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{cleanText}}{
#' Function to removes \\t,\\n and spaces from the text.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{cleanText(plainText)}
#' }
#' \item{\emph{Value}}{
#'
#' The text without \\t,\\n and spaces.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{plainText}}{
#' (character) Text to removes \\t,\\n and spaces.
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
#' @importFrom stringi stri_detect_fixed
#' @importFrom textutils trim
#' @importFrom XML xpathSApply
#' @importFrom XML xmlValue
#' @export StripHTMLPipe

StripHTMLPipe <- R6Class(
    
  "StripHTMLPipe",

  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
     
      if (!"character" %in% class(propertyName)) {
        stop("[StripHTMLPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StripHTMLPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StripHTMLPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },  
    
    pipe = function(instance) {
 
      if (!"Instance" %in% class(instance)) {
        stop("[StripHTMLPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      instance$addFlowPipes("StripHTMLPipe")
      
      if (!instance$checkCompatibility("StripHTMLPipe", self$getAlwaysBeforeDeps())) {
        stop("[StripHTMLPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getData() %>>% 
        self$cleanText() %>>%
          trim() %>>%
            instance$setData()
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StripHTML")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[StripHTMLPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      instance$getData() %>>% 
        self$getDataWithOutHtml() %>>%
          trim() %>>%
            instance$setData()
        
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StripHTML")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[StripHTMLPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance);
    },
    
    getDataWithOutHtml = function(data) {
        
      if (!"character" %in% class(data)) {
        stop("[StripHTMLPipe][getDataWithOutHtml][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      doc <- XML::htmlParse(data ,encoding = "UTF-8", asText = TRUE)
      plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      plain.text2 <- paste0(plain.text, collapse = "") 
      plain.text3 <- self$cleanText(plain.text2)
      
      return(plain.text3)
    },
    
    cleanText = function(plainText) {

      if (!"character" %in% class(plainText)) {
        stop("[StripHTMLPipe][cleanText][Error] 
                Checking the type of the variable: plainText ", 
                  class(plainText))
      }
      
      plainText <- gsub("\\\\t", " ", plainText)
      plainText <- gsub("\\\\n", " ", plainText)
      plainText <- gsub("\\\\r", " ", plainText)
      plainText <- gsub("[[:space:]]+", " ", plainText)
      
      return(plainText)
    }
  )  
)
