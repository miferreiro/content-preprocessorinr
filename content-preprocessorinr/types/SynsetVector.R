#' @title Class to represent a vector of synsets and the asociated information
#' @description A class to represent a vector of synsets and the asociated information.
#' @docType class
#' @usage SynsetVector$new(originalText)
#' @param originalText  (originalText) The start index of an entry.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{getOriginalText}}{
#' Getter of originalText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getOriginalText()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of originalText.
#' }
#' }
#' }
#'
#' \item{\bold{setOriginalText}}{
#' Setter of originalText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setOriginalText(originalText)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{originalText}}{
#' (character) The new value of originalText.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getFixedText}}{
#' Getter of fixedText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getFixedText()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of fixedText.
#' }
#' }
#' }
#'
#' \item{\bold{setFixedText}}{
#' Setter of fixedText.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setFixedText(fixedText)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{fixedText}}{
#' (character) The new value of fixedText.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getUnmatchedTexts}}{
#' Getter of unmatchedTexts.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getUnmatchedTexts()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of unmatchedTexts.
#' }
#' }
#' }
#'
#' \item{\bold{setUnmatchedTexts}}{
#' Setter of unmatchedTexts.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setUnmatchedTexts(unmatchedTexts)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{unmatchedTexts}}{
#' (list) The new value of unmatchedTexts.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getSynsets}}{
#' Getter of synsets.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getSynsets()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of synsets.
#' }
#' }
#' }
#'
#' \item{\bold{setSynsets}}{
#' Setter of synsets.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setSynsets(synsets)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{synsets}}{
#' (list) The new value of synsets.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{originalText}}{
#'  (character) The original text.
#' }
#' \item{\bold{fixedText}}{
#'  (character) The text after fixing unmatched text sections.
#' }
#' \item{\bold{unmatchedTexts}}{
#'  (list) The list of detected synsets represented as Pairs where:
#                       - The name element is the synsetId identified by babelfy
#                       - The value element is the portion of the fixedText that
#                         matches the synsetId
#' }
#' \item{\bold{synsets}}{
#'  (list) The list of detected synsets represented as Pairs where:
#                - The first element of the pair is the synsetId identified by
#                  babelfy
#                - The second element of the pair is the portion of the fixedText
#                  that matches the synsetId
#' }
#'
#'}
#' @import R6
# @export SynsetVector

SynsetVector <- R6Class(
  
  "SynsetVector",
  
  public = list(
    
    initialize = function(originalText) {

      if (!"character" %in% class(originalText)) {
        stop("[SynsetVector][initialize][Error] 
                Checking the type of the variable: originalText ", 
                  class(originalText))
      }

      private$originalText <- originalText
    },
    
    getOriginalText = function() {

      return(private$originalText)
    },
    
    setOriginalText = function(originalText) {

      if (!"character" %in% class(originalText)) {
        stop("[SynsetVector][setOriginalText][Error]
                Checking the type of the variable: originalText ",
                  class(originalText))
      }
      
      private$originalText <- originalText
      
      return()
    },
    
    getFixedText = function() {

      return(private$fixedText)
    },
    
    setFixedText = function(fixedText) {

      if (!"character" %in% class(fixedText)) {
        stop("[SynsetVector][setFixedText][Error]
                Checking the type of the variable: fixedText ",
                  class(fixedText))
      }
      
      private$fixedText <- fixedText
      
      return()
    },
    
    getUnmatchedTexts = function() {

      return(private$unmatchedTexts)
    },
    
    setUnmatchedTexts = function(unmatchedTexts) {

      if (!"list" %in% class(unmatchedTexts)) {
        stop("[SynsetVector][setUnmatchedTexts][Error]
                Checking the type of the variable: unmatchedTexts ",
                  class(unmatchedTexts))
      }
      
      private$unmatchedTexts <- unmatchedTexts
      
      return()
    },
    
    getSynsets = function() {

      return(private$synsets)
    },
    
    setSynsets = function(synsets) {

      if (!"list" %in% class(synsets)) {
        stop("[SynsetVector][setSynsets][Error]
                Checking the type of the variable: synsets ",
                  class(synsets))
      }
      
      private$synsets <- synsets
      
      return()
    }
  ),
  private = list(
    originalText = "",
    fixedText = "",
    unmatchedTexts = list(),
    synsets = list()
  )
)