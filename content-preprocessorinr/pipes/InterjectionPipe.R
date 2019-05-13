#' @title Class to find and/or remove the interjections on the data of an instance
#' @description This class allows you to preprocess the data of an instance to
#' find the interjections that are in it. Optionally, you can decide whether to
#' remove the data interjections or not.
#' @docType class
#' @usage InterjectionPipe$new(propertyName = "interjection",
#'                      propertyLanguageName = "language",
#'                      pathResourcesInterjections = "resources/interjections-json",
#'                      alwaysBeforeDeps = list("GuessLanguagePipe"),
#'                      notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param propertyLanguageName  (character) Name of the language property.
#' @param pathResourcesInterjections (character) Path where are stored the
#'  interjections resources.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details This class needs files in json format that will contain the
#' interjections to be located. For this it is necessary that the instance
#' contains a property that indicates the language of the data to be able to
#' correctly choose the list of interjections that apply to the data. The format
#'  of the file names of the resources has to be: interj.xxx.json (Being xxx the
#'  value of the language property of the instance).
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
#' Function that preprocesses the instance to obtain/remove the interjections.
#' The interjections found in the pipe are added to the list of properties of
#' the Instance. If the removeInterjections parameter is TRUE, the instance data
#' will be removed.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, removeInterjections = TRUE)}
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
#' \item{\strong{removeInterjections}}{
#' (logical) Indicates if the interjections are removed or not.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findInterjection}}{
#' Function that checks if the interjection is in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findInterjection(data, interjection)}{}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE or FALSE depending on whether the interjection is on the data.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{data}}{
#' (character) Text in which the interjection is searched.
#' }
#' \item{\strong{interjection}}{
#' (character) Indicate the interjection to find.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeInterjection}}{
#' Function that removes the interjection in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{removeInterjection(interjection, data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with interjection removed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{interjection}}{
#' (character) Indicates the interjection to remove.
#' }
#' \item{\strong{data}}{
#' (character) Text in which interjections will be removed.
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
#' \item{\bold{getPathResourcesInterjections}}{
#' Getter of path of interjections resources.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPathResourcesInterjections()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path of interjections resources.
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
#' \item{\bold{pathResourcesInterjections}}{
#'  (character) The path where are the resources.
#' }
#' }
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}},
#' \code{\link{ResourceHandler}}
#'
#' @import R6  rlist pipeR
#' @importFrom textutils trim
#' @importFrom rex regex
#' @importFrom rex escape
#' @export InterjectionPipe

InterjectionPipe <- R6Class(
  
  "InterjectionPipe",

  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "interjection",
                          propertyLanguageName = "language",
                          pathResourcesInterjections = "content-preprocessorinr/resources/interjections-json",  
                          alwaysBeforeDeps = list("GuessLanguagePipe"), 
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[InterjectionPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }

      if (!"character" %in% class(propertyLanguageName)) {
        stop("[InterjectionPipe][initialize][Error] 
                Checking the type of the variable: propertyLanguageName ", 
                  class(propertyLanguageName))
      }
      
      if (!"character" %in% class(pathResourcesInterjections)) {
        stop("[InterjectionPipe][initialize][Error] 
                Checking the type of the variable: pathResourcesInterjections ", 
                  class(pathResourcesInterjections))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[InterjectionPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[InterjectionPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
      private$propertyLanguageName <- propertyLanguageName
      private$pathResourcesInterjections <- pathResourcesInterjections
      
    }, 
    
    pipe = function(instance, removeInterjections = TRUE) {
  
      if (!"Instance" %in% class(instance)) {
        stop("[InterjectionPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }

      if (!"logical" %in% class(removeInterjections)) {
        stop("[FindUrlInStringBufferPipe][pipe][Error]
                Checking the type of the variable: removeInterjections ", 
                  class(removeInterjections))
      }      
           
      instance$addFlowPipes("InterjectionPipe")
      
      if (!instance$checkCompatibility("InterjectionPipe", self$getAlwaysBeforeDeps())) {
        stop("[InterjectionPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      languageInstance <- "Unknown"
      
      languageInstance <- instance$getSpecificProperty(self$getPropertyLanguageName()) 
      
      #It is verified that there is a resource associated to the language of the instance
      if (is.null(languageInstance) || 
            is.na(languageInstance) || 
              "Unknown" %in% languageInstance) {

        instance$addProperties(list(), super$getPropertyName()) 
        
        message <- c( "The file: " , instance$getPath() , " has not language property")

        cat("[InterjectionPipe][pipe][Warning] ", message, " \n")
        
        return(instance)
      }
      
      JsonFile <- paste(self$getPathResourcesInterjections(),
                        "/interj.",
                        languageInstance,
                        ".json",
                        sep = "")  
      
      jsonData <- Bdp4R[["private_fields"]][["resourceHandle"]]$isLoadResource(JsonFile)
      
      if (!is.null(jsonData)) { 

        #Variable which stores the interjections located in the data
        interjectionsLocated <- list() 
      
        for (interjection in jsonData) {
          
          if (self$findInterjection(instance$getData(), interjection)) {  
            interjectionsLocated <- list.append(interjectionsLocated, interjection) 
          }
          
          if (removeInterjections && interjection %in% interjectionsLocated) {
            
            instance$getData() %>>%
              {self$removeInterjection(interjection, .)} %>>%
                trim() %>>%
                  instance$setData()
          }  
        }     
        
        instance$addProperties(paste(interjectionsLocated), super$getPropertyName())      
        
      } else {
        
        instance$addProperties(list(), super$getPropertyName()) 
        
        cat("[InterjectionPipe][pipe][Warning] ", 
            "The file: " , instance$getPath() , " has not an interjectionsJsonFile ",
            "to apply to the language ->", languageInstance, " \n")

        return(instance)
      }
      
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Interjection")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[InterjectionPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
            
      return(instance)
    },
    
    findInterjection = function(data, interjection) {

      if (!"character" %in% class(data)) {
        stop("[InterjectionPipe][findInterjections][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      if (!"character" %in% class(interjection)) {
        stop("[InterjectionPipe][findInterjections][Error] 
                Checking the type of the variable: interjection ", 
                  class(interjection))
      }               
      
      interjectionEscaped <- rex::escape(interjection)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)([¡]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")

      return(grepl(pattern = regex(regularExpresion), x = data, perl = T))
    },
    
    removeInterjection = function(interjection, data) {
  
      if (!"character" %in% class(interjection)) {
        stop("[InterjectionPipe][removeInterjection][Error] 
                Checking the type of the variable: interjection ", 
                  class(interjection))
      }               
    
      
      if (!"character" %in% class(data)) {
        stop("[InterjectionPipe][removeInterjection][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
      
      interjectionEscaped <- rex::escape(interjection)
      
      regularExpresion <- paste0("(?:[[:space:]]|[\"><¡?¿!;:,.'-]|^)([¡]*(",
                                 interjectionEscaped,
                                 ")[!]*)[;:?\"!,.'>-]?(?=(?:[[:space:]]|$|>))",
                                 sep = "")
     
      return(gsub(regex(regularExpresion), "", data , perl = T))

    },
    
    getPropertyLanguageName = function() {

      return(private$propertyLanguageName)
    },
    
    getPathResourcesInterjections = function() {

      return(private$pathResourcesInterjections)
    }
  ),
  
  private = list(
    propertyLanguageName = "",
    pathResourcesInterjections = ""
  )
)
