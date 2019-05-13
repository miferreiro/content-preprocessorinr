#' @title Class to get the match with the specific word from an Urban dictionary
#' @description Class to get the match with the specific word from an
#' Urban dictionary.
#' @docType class
#' @usage UrbanDictionaryHandler$new(pathResourcesSlangs = "resources/slangs-json")
#' @param propertyName  (character) Name of the property.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{handle}}{
#' Get the matches with the originalText and set it to replacementText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{handle(originalText, replacementText, lang)}
#' }
#' \item{\emph{Value}}{
#'
#' The word that matches with the originalText.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{originalText}}{
#' (character)  The original text to replace.
#' }
#' \item{\strong{replacementText}}{
#' (character) The word that matches with the originalText.
#' }
#' \item{\strong{lang}}{
#' (character) The language of the original string.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getReplacement4SlangTerm}}{
#' Get the matches with the originalText and set it to replacementText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getReplacement4SlangTerm(slangTerm, lang)}
#' }
#' \item{\emph{Value}}{
#'
#' The word that matches with the originalText.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{slangTerm}}{
#' (character) The original text to replace.
#' }
#' \item{\strong{lang}}{
#' (character) The language of the original string.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPathResourcesSlangs}}{
#' Getter of path of slangs resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPathResourcesSlangs()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path of slangs eesources.
#' }
#' }
#' }
#'
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{pathResourcesSlangs}}{
#'  (character) The path where are the resources.
#' }
#' }
#'
#' @seealso \code{UnmatchedTextHandler}
#'
#' @import R6
#' @export UrbanDictionaryHandler

UrbanDictionaryHandler <- R6Class(
  
  "UrbanDictionaryHandler",
  
  inherit = UnmatchedTextHandler,
  
  public = list(
    
    initialize = function(pathResourcesSlangs = "content-preprocessorinr/resources/slangs-json") {

      if (!"character" %in% class(pathResourcesSlangs)) {
        stop("[UrbanDictionaryHandler][initialize][Error] 
                Checking the type of the variable: pathResourcesSlangs ", 
                  class(pathResourcesSlangs))
      }
      
      private$pathResourcesSlangs <- pathResourcesSlangs
    },
    
    handle = function(originalText, replacementText, lang) {

      if (!"character" %in% class(originalText)) {
        stop("[UrbanDictionaryHandler][handle][Error] 
                Checking the type of the variable: originalText ", 
                  class(originalText))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][handle][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      matchedString <- replacementText
      
      if (is.null(replacementText)) {
        matchedString <- self$getReplacement4SlangTerm(originalText, lang)
        if (!is.null(matchedString)) {
          cat("[UrbanDictionaryHandler][handle][Info]"," Sucessfull match for string ", matchedString, "\n")
        }
      }
      
      return(matchedString)
    },
    
    getReplacement4SlangTerm = function(slangTerm, lang) {
         
      if (!"character" %in% class(slangTerm)) {
        stop("[UrbanDictionaryHandler][getReplacement4SlangTerm][Error] 
                Checking the type of the variable: slangTerm ", 
                  class(slangTerm))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[UrbanDictionaryHandler][getReplacement4SlangTerm][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      JsonFile <- paste(self$getPathResourcesSlangs(),
                        "/slang.",
                        tolower(lang),
                        ".json",
                        sep = "") 
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      if (is.null(jsonData)) { 
        message <- c( "Has not an SlangsJsonFile to apply to the language -> ", tolower(lang))
          
        cat("[UrbanDictionaryHandler][getReplacement4SlangTerm][Warning] ", message, " \n")
        return(NULL)
      }
      
      if (!slangTerm %in% names(jsonData)) {
        return(NULL)
      }
      
      return(jsonData)[[slangTerm]]
    },
      
    getPathResourcesSlangs = function() {

      return(private$pathResourcesSlangs)
    }
  ),
  
  private = list(
    pathResourcesSlangs = ""
  )
)