#' @title Class to find and/or replace the contractions on the data of a instance
#' @description This class allows you to preprocess the data of an instance to
#' find the contractions that are in it. Optionally, you can decide whether to
#' replace the data contractions or not.
#' @docType class
#' @usage ContractionsPipe$new(propertyName = "contractions",
#'                      propertyLanguageName = "language",
#'                      pathResourcesContractions = "resources/contractions-json",
#'                      alwaysBeforeDeps = list("GuessLanguagePipe"),
#'                      notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param propertyLanguageName  (character) Name of the language property.
#' @param pathResourcesContractions (character) Path where are stored the
#  contractions resources.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details This class needs files in json format that will contain the
#' contractions to be located and the string that will replace them. For this
#' it is necessary that the instance contains a property that indicates the
#' language of the data to be able to correctly choose the list of contractions
#' that apply to the data. The format of the file names of the resources has to
#' be: contr.xxx.json (Being xxx the value of the language property of the
#' instance).
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
#' Function that preprocesses the instance to obtain/replace the contractions.
#' The contractions found in the pipe are added to the list of properties of
#' the Instance. If the replaceContractions parameter is TRUE, the instance
#' data will be modified by replacing the contractions found.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, replaceContractions = TRUE)}
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
#' \item{\strong{replaceContractions}}{
#' (logical) Indicates if the contractions are replace or not.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findContraction}}{
#' Function that checks if the contractions is in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findContraction(data, contraction)}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE or FALSE depending on whether the contraction is on the data.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the contraction is searched.
#' }
#' \item{\strong{contraction}}{
#' (character) Indicates the contraction to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{replaceContraction}}{
#' Function that replaces the contraction in the data for the extendedContraction.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{replaceContraction(contraction, extendedContraction, data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with the contractions replaced.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{contraction}}{
#' (character) Indicates the contraction to remove.
#' }
#' \item{\strong{extendedContraction}}{
#' (character) Indicates the string to replace for the contraction found.
#' }
#' \item{\strong{data}}{
#' (character) Text in which contractions will be replaced.
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
#' \item{\bold{getPathResourcesContractions}}{
#' Getter of path of contractions resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPathResourcesContractions()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path of contractions resources.
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
#' \item{\bold{pathResourcesContractions}}{
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
#' @export ContractionsPipe

ContractionsPipe <- R6Class(
  
  "ContractionsPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "contractions", 
                          propertyLanguageName = "language",
                          pathResourcesContractions = "content-preprocessorinr/resources/contractions-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list()) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"character" %in% class(propertyLanguageName)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesContractions)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesContractions ", 
                  class(pathResourcesContractions))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[ContractionsPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesContractions <- pathResourcesContractions
    }, 
    
    pipe = function(instance, replaceContractions = TRUE) {

      if (!"Instance" %in% class(instance)) {
        stop("[ContractionsPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(replaceContractions)) {
        stop("[ContractionsPipe][pipe][Error]
                Checking the type of the variable: replaceContractions ", 
                  class(replaceContractions))
      }  
      
      instance$addFlowPipes("ContractionsPipe")
      
      if (!instance$checkCompatibility("ContractionsPipe", self$getAlwaysBeforeDeps())) {
        stop("[ContractionsPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
      
      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) || 
          is.na(languageInstance) || 
          "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(),super$getPropertyName()) 
        
        cat("[ContractionsPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() ," has not language property\n")
        
        return(instance)
        
      }
      
      JsonFile <- paste(self$getPathResourcesContractions(),
                        "/contr.",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      #It is verified that there is a resource associated to the language of the instance
      if (!is.null(jsonData)) {
        
        #Variable which stores the contractions located in the data
        contractionsLocated <- list()           
        
        for (contraction in names(jsonData)) {
          
          if (self$findContraction(instance$getData(), contraction)) {  
            
            contractionsLocated <- list.append(contractionsLocated, 
                                               contraction) 
          }
          
          if (replaceContractions && contraction %in% contractionsLocated) {
            
            instance$getData() %>>%
              {self$replaceContraction(contraction, 
                                        as.character(jsonData[contraction]),
                                        .)} %>>%
                trim() %>>%
                  instance$setData()
          }
        }     
        
        instance$addProperties(paste(contractionsLocated),
                               super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(),super$getPropertyName()) 
 
        cat("[ContractionsPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() , " has not an contactionsJsonFile ",
            "to apply to the language ->", languageInstance, " \n")
        
        return(instance)
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Contractions")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[ContractionsPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findContraction = function(data, contraction) {

      if (!"character" %in% class(data)) {
        stop("[ContractionsPipe][findContraction][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(contraction)) {
        stop("[ContractionsPipe][findContraction][Error] 
                Checking the type of the variable: contraction ", 
                  class(contraction))
      }               
      
      contractionEscaped <- rex::escape(contraction)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 contractionEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(grepl(pattern = regex(regularExpresion), x = data, perl = T, ignore.case = TRUE))
    },    
    
    replaceContraction = function(contraction, extendedContraction, data) {
   
      if (!"character" %in% class(contraction)) {
        stop("[ContractionsPipe][replaceContraction][Error] 
                Checking the type of the variable: contraction ", 
                  class(contraction))
      }               
      
      if (!"character" %in% class(extendedContraction)) {
        stop("[ContractionsPipe][replaceContraction][Error] 
                Checking the type of the variable: extendedContraction ", 
                  class(extendedContraction))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[ContractionsPipe][replaceContraction][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      contractionEscaped <- rex::escape(contraction)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 contractionEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(gsub(regex(regularExpresion), 
                  paste(" ", extendedContraction, " ", sep = ""), data, perl = T, ignore.case = TRUE))
    },
    
    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },
    
    getPathResourcesContractions = function() {

      return(private$pathResourcesContractions)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesContractions = ""
  )
)
