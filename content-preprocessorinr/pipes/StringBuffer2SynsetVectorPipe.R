#' @title Class to compute synsets from text
#' @description Class to compute synsets from text.
#' @docType class
#' @usage StringBuffer2SynsetVectorPipe$new(propertyName = "synsetVector",
#'                                   propertyLanguageName = "language",
#'                                   alwaysBeforeDeps = list(),
#'                                   notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param propertyLanguageName (character) Name of the language property.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details Building...
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Compute synsets from text. This method get data from StringBuffer and
#' process instances:
#'- Invalidate instance if the language is not present
#'- Get the list of unmatched texts
#'- Process this texts to get matches
#'- Build a synset vector
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
#' (Instance) Instance to preproccess.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{computeUnmatched}}{
#' This method find fagments in text (str) thar are incorrect.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{computeUnmatched(str, lang)}
#' }
#' \item{\emph{Value}}{
#'
#' A list where the name is the incorrect fragment and the value
#' will be the replacement (null now).
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{str}}{
#' (character) The original text.
#' }
#' \item{\strong{lang}}{
#' (character) The language of the original text.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{handleUnmatched}}{
#' Try to fix terms that are incorrectly written. The original text should be
#' fixed according with the replacements made.
#' Implement the UnmatchedTextHandler interface and one specific implementations
#' that are:
#'   + UrbanDictionaryHandler
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{handleUnmatched(originalText, unmatched, lang)}
#' }
#' \item{\emph{Value}}{
#'
#' A string containing the original text fixed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{originalText}}{
#' (character) The originalText to fix.
#' }
#' \item{\strong{unmatched}}{
#' (list) A list of text fragments that should be tryed to fix. The text
#' fragments are in the form of a pair (T,R) where T is the original fragment
#' ant R the replacement (null originally). This method should fill R with the
#' suggested replacement.
#' }
#' \item{\strong{lang}}{
#' (character) The language of the original text.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{buildSynsetVector}}{
#' Create a synsetVector from text. Call Babelfy api to transform the string
#' into a vector of sysnsets. The fisrt string in the pair is the synsetID from
#' babelnet. The second string is the matched text.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{buildSynsetVector(fixedText, lang)}
#' }
#' \item{\emph{Value}}{
#'
#' A list of synsets. Each synset is represented in a pair (S,T) where S stands
#' for the synset ID and T for the text that matches this synset ID.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{fixedText}}{
#' (character) The text to transform into a synset vector.
#' }
#' \item{\strong{lang}}{
#' (character) The language in which the original text is written.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPropertyLanguageName}}{
#' Getter of name of property language.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPropertyLanguageName()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of name of property language.
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{vUTH}}{
#'  (list) An array of UnmatchedTextHandlers to fix incorrect text fragments.
#' }
#' \item{\bold{acceptedCharOnBeggining}}{
#'  (character) List of puntuation marks accepted on the beggining of a word.
#' }
#' \item{\bold{acceptedCharOnBegginingPattern}}{
#'  (character) List of puntuation marks accepted on the beggining of a word.
#' }
#' \item{\bold{acceptedCharOnEnd}}{
#'  (character) List of puntuation marks accepted on the end of a word.
#' }
#' \item{\bold{acceptedCharOnEndPattern}}{
#'  (character) List of puntuation marks accepted on the end of a word.
#' }
#' \item{\bold{acceptedCharOnMiddle}}{
#'  (character) List of puntuation marks accepted on the middle of a word.
#' }
#' \item{\bold{acceptedCharOnMiddlePattern}}{
#'  (character) List of puntuation marks accepted on the middle of a word.
#' }
#' \item{\bold{puntMarkPattern}}{
#'  (character) A pattern to detect puntuation marks.
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{propertyLanguageName}}{
#'  (character) The name of property about language.
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 tools pipeR tokenizers rlist
#' @importFrom rex regex
#' @export StringBuffer2SynsetVectorPipe

StringBuffer2SynsetVectorPipe <- R6Class(
  
  "StringBuffer2SynsetVectorPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "synsetVector", 
                          propertyLanguageName = "language",
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
             
      if (!"character" %in% class(propertyName)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
             Checking the type of the variable: propertyName ", 
             class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
             class(propertyLanguageName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StringBuffer2SynsetVectorPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      
      self$vUTH <- list(UrbanDictionaryHandler$new())
    },
  
    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },
    vUTH = list(),
    acceptedCharOnBeggining = "¿¡[(\"'",
    acceptedCharOnBegginingPattern = "^[¿¡\\[\\(\"'][¿¡\\[\\(\"']*",
    acceptedCharOnEnd = ".,!?)];:\"'",
    acceptedCharOnEndPattern = "[.,!?\\)\\];:<>\"'][.,!?\\)\\];:<>\"']*$",
    acceptedCharOnMiddle = "/-.,;:",
    acceptedCharOnMiddlePattern = "[-\\/\\()\\).,;:<>][-\\/.,;:<>]*",
    puntMarkPattern = "[[:punct:]]",
    
    computeUnmatched = function(str, lang) {
  
      if (!"character" %in% class(str)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: str ", 
                  class(str))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      # The value that will be returned
      returnValue <- list()
      
      st <- tokenize_regex(x = str, pattern = "( |\\t|\\n|\\r|\\u000b|\\f)+", simplify = TRUE)

      for (current in st) {
        
        if (grepl(self$puntMarkPattern, current)) {
          
          indexOfPuntMark <- regexpr(self$puntMarkPattern, current)
          
          if (indexOfPuntMark == 1) { #The puntuation symbol is at the beggining
            if (indexOf(self$acceptedCharOnBeggining, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1) {
              returnValue <- list.append(returnValue, NULL)
              names(returnValue)[length(returnValue)] <- current
            } else {
              
              match <- regexpr(self$acceptedCharOnBegginingPattern, current)
              length <- attr(match, "match.length")

              if (!Bdp4R[["private_fields"]][["babelUtils"]]$isTermInBabelNet(substr(current, match + length, nchar(current)), lang)) {
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
              }
            }
          } else {
            if (indexOfPuntMark == nchar(current)) { #the puntuation symbol is at the end
              if (indexOf(self$acceptedCharOnEnd, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1) {
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
              } else {
                if (!Bdp4R[["private_fields"]][["babelUtils"]]$isTermInBabelNet(substr(current, 1, indexOfPuntMark - 1), lang)) {
                  returnValue <- list.append(returnValue, NULL)
                  names(returnValue)[length(returnValue)] <- current
                }
              }
              
            } else {#The puntuation symbol is in the middle
                if (indexOf(self$acceptedCharOnMiddle, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1 &&
                      indexOf(self$acceptedCharOnEnd, escape(substr(current, indexOfPuntMark, indexOfPuntMark))) == -1) {
                
                returnValue <- list.append(returnValue, NULL)
                names(returnValue)[length(returnValue)] <- current
                
              } else {
                
                match <- regexpr(self$acceptedCharOnEndPattern, current)
                length <- attr(match, "match.length")
                
                if (match == indexOfPuntMark) {
                  if (!Bdp4R[["private_fields"]][["babelUtils"]]$isTermInBabelNet(substr(current, 1, indexOfPuntMark - 1), lang)) {
                    returnValue <- list.append(returnValue, NULL)
                    names(returnValue)[length(returnValue)] <- current
                  }
                } else {
                  
                  match <- regexpr(self$acceptedCharOnMiddlePattern, current)
                  length <- attr(match, "match.length")
                  
                  if (match != -1) {
                    
                    firstElement <- substr(current, 1, match[1] - 1)
                    lastElement <- substr(current, match[1] + length[1], nchar(current))
                    
                    if (!Bdp4R[["private_fields"]][["babelUtils"]]$isTermInBabelNet(firstElement, lang) || 
                        (match[1] + length[1] < nchar(current) && !Bdp4R[["private_fields"]][["babelUtils"]]$isTermInBabelNet(lastElement, lang))) {
                      returnValue <- list.append(returnValue, NULL)
                      names(returnValue)[length(returnValue)] <- current
                    }
                  } else {
                    returnValue <- list.append(returnValue, NULL)
                    names(returnValue)[length(returnValue)] <- current
                  }
                }
              }
            }
          }
        } else {
          # We check if the term current exist in babelnet. 
          if (!Bdp4R[["private_fields"]][["babelUtils"]]$isTermInBabelNet(current, lang)) {
            returnValue <- list.append(returnValue, NULL)
            names(returnValue)[length(returnValue)] <- current
          }
        }
      }
      
      return(returnValue)
        
    },

    handleUnmatched = function(originalText, unmatched, lang) {

      if (!"character" %in% class(originalText)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: originalText ", 
                  class(originalText))
      }
      
      if (!"list" %in% class(unmatched)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: unmatched ", 
                  class(unmatched))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      returnValue <- originalText
      
      for (i in 1:length(unmatched)) {
        if (!length(self$vUTH) == 0) {
          for (x in 1:length(self$vUTH)) {
            
            current <- self$vUTH[[x]]$handle(names(unmatched)[[i]], unmatched[[i]], lang)
            
            if (!is.null(current)) {
              break
            }
          }
        }
        
        if (!is.null(unmatched[[i]])) {
          returnValue <- gsub(pattern = names(unmatched)[[i]], 
                              replacement = unmatched[[i]], 
                              x = returnValue, 
                              fixed = T)
        }
      }
      
      return(returnValue)
    },

    buildSynsetVector = function(fixedText, lang) {
         
      if (!"character" %in% class(fixedText)) {
        stop("[StringBuffer2SynsetVectorPipe][buildSynsetVector][Error] 
                Checking the type of the variable: fixedText ", 
                  class(fixedText))
      }
      
      if (!"character" %in% class(lang)) {
        stop("[StringBuffer2SynsetVectorPipe][buildSynsetVector][Error] 
                Checking the type of the variable: lang ", 
                  class(lang))
      }
      
      returnValue <- list()
      
      returnValue <- Bdp4R[["private_fields"]][["babelUtils"]]$buildSynsetVector(fixedText, lang)

      return(returnValue)
    },  

    pipe = function(instance) {

      if (!"Instance" %in% class(instance)) {
        stop("[StringBuffer2SynsetVectorPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
    
      sv <- SynsetVector$new(instance$getData())
      
      # Invalidate the instance if the language is not present
      # We cannot correctly represent the instance if the language is not present
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not language property")
        instance$invalidate()
        warning(message)  
        
        return(instance)
      }

      sv$setUnmatchedTexts(self$computeUnmatched(sv$getOriginalText(), 
                           toupper(languageInstance)))
      
      if (length(sv$getUnmatchedTexts()) > 0) {
        sv$setFixedText(self$handleUnmatched(sv$getOriginalText(), 
                                             sv$getUnmatchedTexts(),
                                             toupper(languageInstance)))
      } else {
        sv$setFixedText(sv$getOriginalText())
      }
      
      sv$setSynsets(self$buildSynsetVector(sv$getFixedText(),
                                           toupper(languageInstance)))
      
      instance$addProperties(sv, super$getPropertyName())
      
      return(instance);
    }
  ),
  
  private = list(
    propertyLanguageName = ""
  )
)
