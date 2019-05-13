#' @title Class to transform a SynsetVector into a SynsetFeatureVector
#' @description A pipe to transform a \code{SynsetVector} wich contains a list of
#' synsets included in a message into a \code{SynsetFeatureVector} wich compile togeher
#' duplicated features and assign a score for each feature according with a
#' groupingStrategy.
#' @docType class
#' @usage SynsetVector2SynsetFeatureVectorPipe$new(propertyName = "",
#'                                          alwaysBeforeDeps = list(),
#'                                          notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The groupStrategy is one of the following:
#'   - COUNT: indicates the number of times that a synset is observed in the
#'   content.
#'   - BOOLEAN: Indicates if the synset is observed in the content (1) or
#'   not (0).
#'   - FREQUENCY: Indicates the frequency of the synset in the text that is the
#'   count of times that the synset is observed divided by the whole amount of
#'   synsets.
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
#' \code{pipe(instance, groupStrategy = "COUNT")}
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
#' \item{\strong{groupStrategy}}{
#' (character) Strategy to count the synstes. It can be COUNT, BOOLEAN or
#' FREQUENCY.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{countMatchers}}{
#' Converts a synsetVector in a synsetFeatureVector, with the synsetId and
#' the number of times that a synsetId appears in synsetVector.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{countMatchers(synsetVector)}
#' }
#' \item{\emph{Value}}{
#'
#' A synsetFeatureVector with the synsetId and the number of times that a
#' synsetId appears in synsetVector.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{synsetVector}}{
#' (SynsetVector)
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{booleanMatchers}}{
#' Converts a synsetVector in a synsetFeatureVector, with the synsetId and
#' 1 or 0 depends if the synsetId appears in synsetVector.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{booleanMatchers(synsetVector)}
#' }
#' \item{\emph{Value}}{
#'
#' A synsetFeatureVector with the synsetId and 1 or 0 depends if the synsetId
#' appears in synsetVector.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{synsetVector}}{
#' (SynsetVector)
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{frequencyMatchers}}{
#' Converts a synsetVector in a synsetFeatureVector, with the synsetId and the
#' frequency that a synsetId appears in synsetVector.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{frequencyMatchers(synsetVector)}
#' }
#' \item{\emph{Value}}{
#'
#' A synsetFeatureVector with the synsetId and the frequency that a synsetId
#' appears in synsetVector.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{synsetVector}}{
#' (SynsetVector)
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 pipeR rlist
#' @export SynsetVector2SynsetFeatureVectorPipe

SynsetVector2SynsetFeatureVectorPipe <- R6Class(
  
  "SynsetVector2SynsetFeatureVectorPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
    
      if (!"character" %in% class(propertyName)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][initialize][Error] 
             Checking the type of the variable: propertyName ", 
             class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance,
                    groupStrategy = "COUNT") {
      
      if (!"Instance" %in% class(instance)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"character" %in% class(groupStrategy)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: groupStrategy ", 
                  class(groupStrategy))
      }

      if (!groupStrategy %in% c("COUNT", "BOOLEAN","FREQUENCY")) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][pipe][Error] 
                Checking the value of the variable: groupStrategy ", 
                  class(groupStrategy))
      }
      
      
      synsetVector <- instance$getSpecificProperty("synsetVector")
      
      # Generate a synsetFeatureVector with synsetId and synsetId appearance number in synsetVector
      if (groupStrategy %in% "COUNT") {
        instance$addProperties(self$countMatchers(synsetVector), "synsetFeatureVector")
      } 
      # Generate a synsetFeatureVector with synsetId and synsetId appearance frequency in synsetVector
      if (groupStrategy %in% "BOOLEAN") {
        instance$addProperties(self$booleanMatchers(synsetVector), "synsetFeatureVector")
      }
      # Generate a synsetFeatureVector with synsetId and synsetId appearance frequency in synsetVector
      if (groupStrategy %in% "FREQUENCY") {
        instance$addProperties(self$frequencyMatchers(synsetVector), "synsetFeatureVector")
      }
      
      return(instance)
    },
    
    countMatchers = function(synsetVector) {

      if (!"SynsetVector" %in% class(synsetVector)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][countMatchers][Error] 
                Checking the type of the variable: synsetVector ", 
                  class(synsetVector))
      }
      
      synsetFeatureVector <- list()    
     
      synsets <- synsetVector$getSynsets()
      
      if (length(synsets) > 0) {
        for (position in 1:length(synsets)) {
          
          synset <- names(synsets)[[position]]
  
          if (!synset %in% names(synsetFeatureVector)) {
            synsetFeatureVector <- list.append(synsetFeatureVector, 1)
            names(synsetFeatureVector)[length(synsetFeatureVector)] <- synset
          } else {
            synsetFeatureVector[[synset]] <- synsetFeatureVector[[synset]] + 1
          }
        }
      } else {
        cat("[SynsetVector2SynsetFeatureVectorPipe][countMatchers][Warning]", 
            "synsets is empty","\n")
      }
      
      return(SynsetFeatureVector$new(synsetFeatureVector))
    },
    
    booleanMatchers = function(synsetVector) {
            
      if (!"SynsetVector" %in% class(synsetVector)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][booleanMatchers][Error] 
                Checking the type of the variable: synsetVector ", 
                  class(synsetVector))
      }
      
      synsetFeatureVector <- list()    
      
      synsets <- synsetVector$getSynsets()
      
      for (position in 1:length(synsets)) {
        
        synset <- names(synsets)[[position]]
          
        if (!synset %in% names(synsetFeatureVector)) {
            synsetFeatureVector <- list.append(synsetFeatureVector, 1)
            names(synsetFeatureVector)[length(synsetFeatureVector)] <- synset
        }
      }
      
      return(SynsetFeatureVector$new(synsetFeatureVector))
    },
    
    frequencyMatchers = function(synsetVector) {

      if (!"SynsetVector" %in% class(synsetVector)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][frequencyMatchers][Error] 
                Checking the type of the variable: synsetVector ", 
                  class(synsetVector))
      }
      
      synsetFeatureVectorCountMatches <- self$countMatchers(synsetVector)
      
      synsets <- synsetFeatureVectorCountMatches$getSynsetsFeature()
      
      countSynsets <- length(synsets)
      
      for (position in 1:countSynsets) {
        synsets[[synset]] <- synsets[[synset]] / countSynsets
      }
      
      return(synsets)
    }
  )
)