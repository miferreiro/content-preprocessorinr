#' @title Class to establish the flow of pipes
#' @description Class to establish the flow of pipes.
#' @docType class
#' @usage TypePipe$new()
#' @details Building...
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{pipeAll}}{
#' Function where the flow of the pipes is created.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipeAll(instance)}
#' }
#' \item{\emph{Value}}{
#'
#' The preprocessed instance.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{instance}}{
#' (Instance) The instance that is going to be processed.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{Instance}}
#'
#' @import R6
#' @export TypePipe

TypePipe <- R6Class(
  
  "TypePipe",
  
  public = list(
    
    initialize = function() {
      
    },
    
    pipeAll = function(instance) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[TypePipe][pipeAll][Error] 
                Checking the type of the variable: instance ", 
                  class(instance));
      }
      
      instance %>|%
        TargetAssigningPipe$new()$pipe() %>|%
        StoreFileExtensionPipe$new()$pipe() %>|%
        GuessDatePipe$new()$pipe() %>|%
        File2Pipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_before_cleaning_text") %>|%
        StripHTMLPipe$new()$pipe()  %>|%
        FindUserNamePipe$new()$pipe() %>|%
        FindHashtagPipe$new()$pipe() %>|%
        FindUrlPipe$new()$pipe() %>|%
        FindEmoticonPipe$new()$pipe() %>|%
        FindEmojiPipe$new()$pipe() %>|%
        GuessLanguagePipe$new()$pipe() %>|%
        ContractionsPipe$new()$pipe() %>|%
        AbbreviationPipe$new()$pipe() %>|%
        SlangPipe$new()$pipe() %>|%
        ToLowerCasePipe$new()$pipe() %>|%
        InterjectionPipe$new()$pipe() %>|%
        StopWordPipe$new()$pipe() %>|%
        MeasureLengthPipe$new()$pipe("length_after_cleaning_text") %>|%
        TeeCSVPipe$new()$pipe() #%>|%
        # StringBuffer2SynsetVectorPipe$new()$pipe() %>|%
        # SynsetVector2SynsetFeatureVectorPipe$new()$pipe() %>|%
        # TeeCSVFromSynsetFeatureVectorPipe$new()$pipe()
      
      return(instance)
    }
  )
)