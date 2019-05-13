#' @title Class to represent a vector of synset-based features
#' @description Class to represent a vector of synset-based features.
#' @docType class
#' @usage SynsetFeatureVector$new(synsetFeature)
#' @param synsetFeature  (list) A map of synsets together with its values.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{getSynsetsFeature}}{
#' Getter of synsetFeature.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getSynsetsFeature()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of synsetFeature.
#' }
#' }
#' }
#'
#' \item{\bold{getSize}}{
#' Gets the size (number of properties) of the current SynsetFeatureVector.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getSize()}
#' }
#' \item{\emph{Value}}{
#'
#' The size of the current SynsetFeatureVector.
#' }
#' }
#' }
#'
#' \item{\bold{getFrequencyValue}}{
#' Checks for the value stored for the synset synsetId.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getFrequencyValue(synsetId)}
#' }
#' \item{\emph{Value}}{
#'
#' The value asociated to synsetId, which represents the frequency of appearance
#' of the synsetId. If the synset is not found, -1 is returned.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{synsetId}}{
#' (character) The target synset.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{synsetFeature}}{
#'  (list) A map of synsets together with its values
#' }
#' }
#'
#' @import R6
# @export SynsetFeatureVector
SynsetFeatureVector <- R6Class(
  
  "SynsetFeatureVector",
  
  public = list(
    
    initialize = function(synsetFeature) {
      
      private$synsetFeature <- synsetFeature
    },
    
    getSynsetsFeature = function() {

      return(private$synsetFeature)
    },
    
    getSize = function() {

      return(length(self$getSynsetsFeature()))
    },
    
    getFrequencyValue = function(synsetId) {
    
      if ( synsetId %in% names(self$getSynsetsFeature())) {
        return(self$getSynsetsFeature()[[synsetId]])
      } else {
        return(-1)
      }
    }
  ),
  private = list(
    synsetFeature = list()
  )
)
