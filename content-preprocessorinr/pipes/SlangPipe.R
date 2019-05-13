#' @title Class to find and/or replace the slangs on the data of an instance
#' @description This class allows you to preprocess the data of an instance to
#' find the slangs that are in it. Optionally, you can decide whether to
#' delete the data slangs or not.
#' @docType class
#' @usage SlangPipe$new(propertyName = "langpropname",
#'               propertyLanguageName = "language",
#'               pathResourcesSlangs = "resources/slangs-json",
#'               alwaysBeforeDeps = list("GuessLanguagePipe"),
#'               notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param propertyLanguageName  (character) Name of the language property.
#' @param pathResourcesSlangs (character) Path where are stored the slangs
#' resources.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details This class needs files in json format that will contain the slangs
#' to be located and the string that will replace them. For this it is necessary
#' that the instance contains a property that indicates the language of the data
#' to be able to correctly choose the list of slangs that apply to the data.
#' The format of the file names of the resources has to be: slang.xxx.json
#' (Being xxx the value of the language property of the instance).
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
#' Function that preprocesses the instance to obtain/replace the slangs.
#' The slangs found in the pipe are added to the list of properties of the
#' Instance. If the replaceSlangs parameter is TRUE, the instance data will be
#' modified by replacing the slangs found.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, replaceSlangs = TRUE)}
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
#' \item{\strong{replaceSlangs}}{
#' (logical) Indicate if the slangs are replace or not.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findSlang}}{
#' Function that checks if the slang is in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findSlang(data, slang)}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE or FALSE depending on whether the slang is on the data.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the slang is searched.
#' \item{\strong{slang}}{
#' (character) Indicates the slang to find.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{replaceSlang}}{
#' Function that replaces the slang in the data for the extendedSlang.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{replaceSlang(slang, extendedSlang, data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with slangs replaced.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{slang}}{
#' (character) Indicates the slang to replace.
#' }
#' \item{\strong{extendedSlang}}{
#' (character) Indicates the string to replace for the slangs found.
#' }
#' \item{\strong{data}}{
#' (character) Text in which slangs will be replaced.
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
#' \item{\bold{getPathResourcesSlangs}}{
#' Getter of path of slangs resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPathResourcesSlangs()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path of slangs resources.
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
#' \item{\bold{pathResourcesSlangs}}{
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
#' @export SlangPipe

SlangPipe <- R6Class(
  
  "SlangPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "langpropname", 
                          propertyLanguageName = "language",
                          pathResourcesSlangs = "content-preprocessorinr/resources/slangs-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list()) {
  
      if (!"character" %in% class(propertyName)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesSlangs)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesSlangs ", 
                  class(pathResourcesSlangs))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[SlangPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesSlangs <- pathResourcesSlangs
    }, 
    
    pipe = function(instance, replaceSlangs = TRUE) {
       
      if (!"Instance" %in% class(instance)) {
        stop("[SlangPipe][pipe][Error]
               Checking the type of the variable: instance ", 
                class(instance))
      }
      
      if (!"logical" %in% class(replaceSlangs)) {
        stop("[SlangPipe][pipe][Error]
                Checking the type of the variable: replaceSlangs ", 
                  class(replaceSlangs))
      }  
      
      instance$addFlowPipes("SlangPipe")
      
      if (!instance$checkCompatibility("SlangPipe", self$getAlwaysBeforeDeps())) {
        stop("[SlangPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      #It is verified that there is a resource associated to the language of the instance
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: ", instance$getPath(), " has not language property")
        
        cat("[SlangPipe][pipe][Warning] ", message, " \n")

        return(instance)
      }
      
      JsonFile <- paste(self$getPathResourcesSlangs(),
                        "/slang.",
                        languageInstance,
                        ".json",
                        sep = "") 
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      if (!is.null(jsonData)) { 
        
        #Variable which stores the Slangs located in the data
        slangsLocated <- list()           
        
        for (slang in names(jsonData)) {
          
          if (self$findSlang(instance$getData(), slang)) {  
            slangsLocated <- list.append(slangsLocated, slang) 
          }
          
          if (replaceSlangs && slang %in% slangsLocated) {
            instance$getData() %>>%
              {self$replaceSlang(slang, as.character(jsonData[slang]), .)} %>>%
                trim() %>>%
                  instance$setData()
          }
        }     
        
        instance$addProperties(paste(slangsLocated), super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: ", instance$getPath(), " has not an SlangsJsonFile to apply to the language-> ", languageInstance )
       
        cat("[SlangPipe][pipe][Warning] ", message, " \n")
        

        return(instance)
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: ", instance$getPath(), " has data empty on pipe Slang")
        
        instance$addProperties(message, "reasonToInvalidate")   

        cat("[SlangPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findSlang = function(data, slang) {
       
      if (!"character" %in% class(data)) {
        stop("[SlangPipe][findSlang][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(slang)) {
        stop("[SlangPipe][findSlang][Error] 
                Checking the type of the variable: slang ", 
                  class(slang))
      }               
      
      slangEscaped <- rex::escape(slang)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.']|^)(", 
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")
    
      return(grepl(pattern = regex(regularExpresion), x = data, perl = T, ignore.case = TRUE))
    },    
    
    replaceSlang = function(slang, extendedSlang, data) {
      
      if (!"character" %in% class(slang)) {
        stop("[SlangPipe][replaceSlang][Error] 
                Checking the type of the variable: slang ", 
                  class(abbreviation))
      }               
      
      if (!"character" %in% class(extendedSlang)) {
        stop("[SlangPipe][replaceSlang][Error] 
                Checking the type of the variable: extendedSlang ", 
                  class(extendedSlang))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[SlangPipe][replaceSlang][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }

      slangEscaped <- rex::escape(slang)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.']|^)(", 
                                 slangEscaped,
                                 ")[;:?\"!,.'>]*(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(gsub(regex(regularExpresion), 
                          paste(" ", extendedSlang, " ", sep = ""), data, perl = T, ignore.case = TRUE))
      
    },
    
    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },
    
    getPathResourcesSlangs = function() {

      return(private$pathResourcesSlangs)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesSlangs = ""
  )
)
