#' @title Class to find and/or replace the abbreviations on the data of an instance
#' @description This class allows you to preprocess the data of an instance to
#' find the abbreviations that are in it. Optionally, you can decide whether to
#' replace the data abbreviations or not.
#' @docType class
#' @usage AbbreviationPipe$new(propertyName = "abbreviation",
#'                      propertyLanguageName = "language",
#'                      pathResourcesAbbreviations = "resources/abbreviations-json",
#'                      alwaysBeforeDeps = list("GuessLanguagePipe"),
#'                      notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param propertyLanguageName  (character) Name of the language property.
#' @param pathResourcesAbbreviations (character) Path where are stored the
#  abbreviation resources.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details This class needs files in json format that will contain the
#' abbreviations to be located and the string that will replace them. For this,
#' it is necessary that the instance contains a property that indicates the
#' language of the data to be able to correctly choose the list of abbreviations
#' that apply to the data. The format of the file names of the resources has to
#' be: abbrev.xxx.json (Being xxx the value of the language property of the
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
#' Function that preprocesses the instance to obtain/replace the abbreviations.
#' The abbreviations found in the pipe are added to the list of properties of
#' the Instance. If the replaceAbbreviations parameter is TRUE, the instance
#' data will be modified by replacing the abbreviations found.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, replaceAbbreviations = TRUE)}
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
#' \item{\strong{replaceAbbreviations}}{
#' (logical) Indicate if the abbreviations are replaced or not.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findAbbreviation}}{
#' Function that checks if the abbreviation is in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findAbbreviation(data, abbreviation)}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE or FALSE depending on whether the abbreviation is in the data.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the abbreviation is searched.
#' }
#' \item{\strong{abbreviation}}{
#' (character) Indicates the abbreviation to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{replaceAbbreviation}}{
#' Function that replace the abbreviation in the data for the extendedAbbreviation.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{replaceAbbreviation(abbreviation, extendedAbbreviation, data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with the abbreviatons replaced.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{abbreviation}}{
#' (character) Indicates the abbreviation to replace.
#' }
#' \item{\strong{extendedAbbreviation}}{
#' (character) Indicates the string to replace for the abbreviations found.
#' }
#' \item{\strong{data}}{
#' (character) Text in which abbreviations will be replaced.
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
#' \item{\bold{getPathResourcesAbbreviations}}{
#' Getter of path of abbreviations resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPathResourcesAbbreviations()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path of abbreviations resources.
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
#' \item{\bold{pathResourcesAbbreviations}}{
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
#' @export AbbreviationPipe

AbbreviationPipe <- R6Class(
  
  "AbbreviationPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "abbreviation", 
                          propertyLanguageName = "language",
                          pathResourcesAbbreviations = "content-preprocessorinr/resources/abbreviations-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list()) {
      
      if (!"character" %in% class(propertyName)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesAbbreviations)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesAbbreviations ", 
                  class(pathResourcesAbbreviations))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[AbbreviationPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  Wclass(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesAbbreviations <- pathResourcesAbbreviations
    }, 
    
    pipe = function(instance, replaceAbbreviations = TRUE) {
 
      if (!"Instance" %in% class(instance)) {
        stop("[AbbreviationPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(replaceAbbreviations)) {
        stop("[AbbreviationPipe][pipe][Error]
                Checking the type of the variable: replaceAbbreviations ", 
                  class(replaceAbbreviations))
      }  
      
      instance$addFlowPipes("AbbreviationPipe")
      
      if (!instance$checkCompatibility("AbbreviationPipe", self$getAlwaysBeforeDeps())) {
        stop("[AbbreviationPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty( self$getPropertyLanguageName()) 
          
      # If the language property is not found, the instance can not be preprocessed
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {
        
        instance$addProperties(list(),super$getPropertyName()) 
    
        cat("[AbbreviationPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() ," has not language property\n")
        
        return(instance)
      }
      
      JsonFile <- paste(self$getPathResourcesAbbreviations(),
                        "/abbrev.",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      #It is verified that there is a resource associated to the language of the instance
      if (!is.null(jsonData)) {
        
        #Variable which stores the abbreviations located in the data
        abbreviationsLocated <- list()           
        
        for (abbreviation in names(jsonData)) {

          if (self$findAbbreviation(instance$getData(), abbreviation)) {  
            abbreviationsLocated <- list.append(abbreviationsLocated, 
                                                  abbreviation) 
          }
          
          if (replaceAbbreviations && abbreviation %in% abbreviationsLocated) {
            
              instance$getData() %>>%
                {self$replaceAbbreviation(abbreviation, 
                                            as.character(jsonData[abbreviation]),
                                              .)} %>>%
                  trim() %>>%
                    instance$setData()
          }
        }     

        instance$addProperties(paste(abbreviationsLocated), super$getPropertyName()) 
        
      } else {
        
        instance$addProperties(list(), super$getPropertyName()) 

        cat("[AbbreviationPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() , " has not an abbreviationsJsonFile ",
            "to apply to the language ->", languageInstance, " \n")
        
        return(instance)
      }


      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Abbreviation")
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[AbbreviationPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        return(instance)
      }
      
      return(instance)
    },

    findAbbreviation = function(data, abbreviation) {

      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][findAbbreviation][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationPipe][findAbbreviation][Error] 
                Checking the type of the variable: abbreviation ", 
                  class(abbreviation))
      }               

      abbreviationEscaped <- rex::escape(abbreviation)

      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
      
      return(grepl(pattern = regex(regularExpresion), x = data, perl = T))
    },    
        
    replaceAbbreviation = function(abbreviation, extendedAbbreviation, data) {
       
      if (!"character" %in% class(abbreviation)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: abbreviation ", 
                  class(abbreviation))
      }               
        
      if (!"character" %in% class(extendedAbbreviation)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: extendedAbbreviation ", 
                  class(extendedAbbreviation))
      }       
      
      if (!"character" %in% class(data)) {
        stop("[AbbreviationPipe][replaceAbbreviation][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }

      abbreviationEscaped <- rex::escape(abbreviation)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)(", 
                                 abbreviationEscaped,
                                 ")[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(gsub(regex(regularExpresion), 
                             paste(" ", extendedAbbreviation, " ", sep = ""), data, perl = T))
    },
    
    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },
    
    getPathResourcesAbbreviations = function() {

      return(private$pathResourcesAbbreviations)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesAbbreviations = ""
  )
)
