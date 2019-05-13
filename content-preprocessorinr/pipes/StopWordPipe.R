#' @title Class to find and/or remove the stop words on the data of an instance
#' @description This class allows you to preprocess the data of an instance to
#' find the stop words that are in it. Optionally, you can decide whether to
#' remove the data stop words or not.
#' @docType class
#' @usage StopWordPipe$new(propertyName = "stopWord",
#'                  propertyLanguageName = "language",
#'                  pathResourcesStopWords = "resources/stopwords-json",
#'                  alwaysBeforeDeps = list("GuessLanguagePipe"),
#'                  notAfterDeps = list("AbbreviationPipe"))
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param propertyLanguageName  (character) Name of the language property.
#' @param pathResourcesStopWords (character) Path where are stored the
#' stop words resources.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details This class needs files in json format that will contain the
#' stop words to be located. For this it is necessary that the instance contains
#' a property that indicates the language of the data to be able to correctly
#' choose the list of stop words that apply to the data. The format of the file
#' names of the resources has to be: xxx.json (Being xxx the value of the
#' language property of the instance).
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
#' Function that preprocesses the instance to obtain/remove the stop words.
#' The stop words found in the pipe are added to the list of properties of
#' the Instance. If the removeStopWords parameter is TRUE, the instance data
#' will be removed.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, removeStopWords = TRUE)}
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
#' \item{\strong{removeStopWords}}{
#' (logical) Indicates if the stop words are removed or not.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findStopWord}}{
#' Function that checks if the stop word is in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findStopWord(data, stopWord)}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE or FALSE depending on whether the stop word is on the data.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the stop word is searched.
#' }
#' \item{\strong{stopWord}}{
#' (character) Indicates the stop word to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeStopWord}}{
#' Function that removes the stop word in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{removeStopWord(stopWord, data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with stop word removed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{stopWord}}{
#' (character) Indicates the stop word to remove.
#' }
#' \item{\strong{data}}{
#' (character) Text in which stop words will be removed.
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
#'
#' \item{\bold{getPathResourcesStopWords}}{
#' Getter of path of stop words resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPathResourcesStopWords()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path of stop words resources.
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{propertyLanguageName}}{
#'  (character) The name of property about language.
#' }
#' \item{\bold{pathResourcesStopWords}}{
#'  (character) The path where are the resources.
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}},
#' \code{\link{ResourceHandler}}
#'
#' @import R6  rlist pipeR
#' @importFrom textutils trim
#' @importFrom rex regex
#' @importFrom rex escape
#' @export StopWordPipe

StopWordPipe <- R6Class(
  
  "StopWordPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "stopWord",
                          propertyLanguageName = "language",
                          pathResourcesStopWords = "content-preprocessorinr/resources/stopwords-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list("AbbreviationPipe")) {
         
      if (!"character" %in% class(propertyName)) {
        stop("[StopWordPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[StopWordPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesStopWords)) {
        stop("[StopWordPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesStopWords ", 
                  class(pathResourcesStopWords))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[StopWordPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[StopWordPipe][initialize][Error] 
                 Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesStopWords <- pathResourcesStopWords
    }, 
        
    pipe = function(instance, removeStopWords = TRUE) {
        
      if (!"Instance" %in% class(instance)) {
        stop("[StopWordPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }

      if (!"logical" %in% class(removeStopWords)) {
        stop("[StopWordPipe][pipe][Error]
                Checking the type of the variable: removeStopWords ", 
                  class(removeStopWords))
      }    
      
      instance$addFlowPipes("StopWordPipe")
      
      if (!instance$checkCompatibility("StopWordPipe", self$getAlwaysBeforeDeps())) {
        stop("[StopWordPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <-
          paste("The file: " ,
                instance$getPath() ,
                " has not language property",
                sep = "")
        
        cat("[StopWordPipe][pipe][Warning] ", message, " \n")
        
        
        return(instance)
      }      

      JsonFile <- paste(self$getPathResourcesStopWords(),
                        "/",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      if (!is.null(jsonData)) { 
        
        #Variable which stores the stopwords located in the data
        stopWordLocated <- list()
      
        for (stopWord in jsonData) {
          
          if (self$findStopWord(instance$getData(), stopWord)) {  
            stopWordLocated <- list.append(stopWordLocated, stopWord) 
          }         
          
          if (removeStopWords && stopWord %in% stopWordLocated) { 

            instance$getData() %>>%
              {self$removeStopWord(stopWord, .)} %>>%
                trim() %>>%
                  instance$setData()
          }  
        } 
             
        instance$addProperties(paste(stopWordLocated),
                                super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <-
          paste(
            "The file: " ,
            instance$getPath() ,
            " has not an StopWordsJsonFile to apply to the language-> ",
            languageInstance,
            sep = ""
          )

        cat("[StopWordPipe][pipe][Warning] ", message, " \n")
        
        return(instance) 
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe StopWord")
        
        instance$addProperties(message, "reasonToInvalidate")  
        
        cat("[StopWordPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
        
      return(instance)
    },

    findStopWord = function(data, stopWord) {
       
      if (!"character" %in% class(data)) {
        stop("[StopWordPipe][findStopWord][Error] 
                Checking the StopWordPipe of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(stopWord)) {
        stop("[StopWordPipe][findStopWord][Error] 
                Checking the type of the variable: stopWord ", 
                  class(stopWord))
      }               
      
      stopWordEscaped <- rex::escape(stopWord)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(" , 
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(grepl(pattern = regex(regularExpresion), x = data , perl = T))
      
    },    
        
    removeStopWord = function(stopWord, data) {
        
      if (!"character" %in% class(stopWord)) {
        stop("[StopWordPipe][removeStopWord][Error] 
                Checking the type of the variable: stopWord ", 
                  class(stopWord))
      }               
      
      if (!"character" %in% class(data)) {
        stop("[StopWordPipe][removeStopWord][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      stopWordEscaped <- rex::escape(stopWord)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(" , 
                                 stopWordEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(gsub(regex(regularExpresion),"", data, perl = T))
    },
    
    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },
    
    getPathResourcesStopWords = function() {

      return(private$pathResourcesStopWords)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesStopWords = ""
  )
)
