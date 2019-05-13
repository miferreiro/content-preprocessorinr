#' @title Class to guess the language
#' @description Class to guess the language by using language detector of library
#' cld2. Optionally, it is possible to choose the language provided by twitter.
#' @docType class
#' @usage GuessLanguagePipe$new(propertyName = "language",
#'                       alwaysBeforeDeps = list("StoreFileExtensionPipe",
#'                                               "TargetAssigningPipe",
#'                                               "StripHTMLPipe"),
#'                       notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The pipe will invalidate the instance if the language of the data
#' can not be detect.
#'
#' To obtain the language of the tweets, it will be verified that there is a
#' json file with the information stored in memory.
#'
#' @section Inherit:
#' This class inherits from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that preprocesses the instance to obtain the language of the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, languageTwitter = TRUE)}
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
#' \item{\strong{languageTwitter}}{
#' (logical) Indicates whether for the instances of type twtid the language that
#' returns the api is obtained or the detector is applied.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getLanguage}}{
#' Function that guess the language of data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getLanguage(data)}
#' }
#' \item{\emph{Value}}{
#'
#' The language guesser. Format: see ISO 639-3:2007.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text to guess the language.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 cld2 pipeR
#' @importFrom rjson fromJSON
#' @export GuessLanguagePipe

GuessLanguagePipe <- R6Class(
    
  "GuessLanguagePipe",
 
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "language",  
                          alwaysBeforeDeps = list("StoreFileExtensionPipe", 
                                                  "TargetAssigningPipe",
                                                  "StripHTMLPipe"), 
                          notAfterDeps = list()) {
           
      if (!"character" %in% class(propertyName)) {
        stop("[GuessLanguagePipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[GuessLanguagePipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[GuessLanguagePipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    }, 
    
    pipe = function(instance, languageTwitter = TRUE) {
          
      if (!"Instance" %in% class(instance)) {
        stop("[GuessLanguagePipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
        
      instance$addFlowPipes("GuessLanguagePipe")
      
      if (!instance$checkCompatibility("GuessLanguagePipe", self$getAlwaysBeforeDeps())) {
        stop("[GuessLanguagePipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      if (languageTwitter 
            && instance$getSpecificProperty("extension") %in% "twtid") {

        if (file.exists(paste("content-preprocessorinr/testFiles/cache/hsspam14/",
                                "tweets/_", 
                                  instance$getSpecificProperty("target"), 
                                    "_/", 
                                      instance$getId(), 
                                        ".json", 
                                          sep = ""))) {
          
          path <- paste("content-preprocessorinr/testFiles/cache/hsspam14/tweets/_",
                          instance$getSpecificProperty("target"), 
                            "_/", 
                              instance$getId(), 
                                ".json", 
                                  sep = "")
          
          dataFromJsonFile <- fromJSON(file = path)
          
          if (!is.na(dataFromJsonFile[["lang"]]) && 
                !is.null(dataFromJsonFile[["lang"]]) 
                  && dataFromJsonFile[["lang"]] != "") {
            
            langTwitter <- dataFromJsonFile[["lang"]]
            
            instance$addProperties(langTwitter,super$getPropertyName())
            
            if (is.null(instance$getSpecificProperty("language"))) {
              
              message <- c( "The file: " , instance$getPath() , " has a NULL twitter language")
              
              instance$addProperties(message, "reasonToInvalidate") 
              
              cat("[GuessLanguagePipe][pipe][Warning] ", message, " \n")
              
              instance$invalidate()
              
              return(instance)
            }
          
            return(instance)
          } 
        }
      } 
      
      instance$getData() %>>% 
        self$getLanguage() %>>%
          {instance$addProperties(.,super$getPropertyName())}
      
      if (is.na(instance$getSpecificProperty("language"))) {
        message <- c( "The file: " , instance$getPath() , " has a null language")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[GuessLanguagePipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    getLanguage = function(data) {
 
      if (!"character" %in% class(data)) {
        stop("[GuessLanguagePipe][getLanguage][Error]
                Checking the type of the variable: data ",
                  class(data))
      }

      langStandardize <- detect_language(data, plain_text = TRUE)

      return(langStandardize)
    }
  )  
)
