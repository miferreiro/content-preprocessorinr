#' @title Class to find and/or remove the urls on the data
#' @description This class allows you to preprocess the data of an instance to
#' find the urls that are in it. Optionally, you can decide whether to
#' remove the data urls or not.
#' @docType class
#' @usage FindUrlPipe$new(propertyName = "URLs",
#'                 alwaysBeforeDeps = list(),
#'                 notAfterDeps = list("FindUrlPipe"))
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details The regular expressions indicated in the \code{URLPatterns}
#' variable are used to identify urls.
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
#' Function that preprocesses the instance to obtain/remove the users.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance,
#'      removeUrl = TRUE,
#'      URLPatterns = list(self$URLPattern, self$EmailPattern),
#'      namesURLPatterns = list("UrlPattern","EmailPattern"))}
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
#' \item{\strong{removeUrl}}{
#' (logical) Indicates if the urls are removed.
#' }
#' \item{\strong{URLPatterns}}{
#' (list) The regex to find urls.
#' }
#' \item{\strong{namesURLPatterns}}{
#' (list) The names of regex.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{findUrl}}{
#' Function that find the urls in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{findHashtag(pattern, data)}
#' }
#' \item{\emph{Value}}{
#'
#' List with urls found.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{pattern}}{
#' (character) Regex to find urls.
#' }
#' \item{\strong{data}}{
#' (character) Text to find urls.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{removeUrl}}{
#' Function that removes the urls in the data.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{removeUrl(pattern, data)}
#' }
#' \item{\emph{Value}}{
#'
#' The data with urls removed.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{pattern}}{
#' (character) Regex to find urls.
#' }
#' \item{\strong{data}}{
#' (character) Text in which hashtags will be urls.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{putNamesURLPattern}}{
#' Set the names to url patterns result.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{putNamesURLPattern(resultOfURLPatterns)}
#' }
#' \item{\emph{Value}}{
#'
#' Value of resultOfURLPatterns variable with the names of url pattern.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{resultOfURLPatterns}}{
#' (list) List with urls found.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getURLPatterns}}{
#' Getter of url patterns.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getURLPatterns()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of url patterns.
#' }
#' }
#' }
#'
#' \item{\bold{getNamesURLPatterns}}{
#' Getter of name of urls.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getNamesURLPatterns()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of name of urls.
#' }
#' }
#' }
#' }
#'
#' @section Public fields:
#' \itemize{
#' \item{\bold{URLPattern}}{
#'  (character) Regular expression to detect urls.
#' }
#' \item{\bold{EmailPattern}}{
#'  (character) Regular expression to detect emails.
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{URLPatterns}}{
#'  (list) Regular expressions used to detect urls.
#' }
#' \item{\bold{namesURLPatterns}}{
#'  (list) Names of regular expressions that are used to identify urls.
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6 rlist pipeR
#' @importFrom textutils trim
#' @importFrom rex regex
#' @importFrom stringr str_match_all
#' @importFrom stringr str_replace_all
#' @export FindUrlPipe

FindUrlPipe <- R6Class(
    
  "FindUrlPipe",
  
  inherit = PipeGeneric,
    
  public = list(

    initialize = function(propertyName = "URLs",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list("FindUserNamePipe")) {
   
      if (!"character" %in% class(propertyName)) {
        stop("[FindUrlPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[FindUrlPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[FindUrlPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },  

    URLPattern = "(?:\\s|[\"><¡¿?!;:,.'\\(]|^)((?:(?:[[:alnum:]]+:(?:\\/{1,2}))|\\/{0,2}www\\.)(?:[\\w-]+(?:(?:\\.[\\w-]+)*))(?:(?:[\\w~?=-][.;,@?^=%&:\\/~+#-]?)*)[\\w@?^=%&\\/~+#,;!:<\\\\\"?-]?(?=(?:[<\\\\,;!\"?\\)]|\\s|$)))",

    EmailPattern = "(?:\\s|[\"><¡¿?!;:,.'\\(]|^)((?:[\\w_.çñ+-]+)(?:@|\\(at\\)|<at>)(?:(?:\\w[\\\\.:ñ-]?)*)[[:alnum:]ñ](?:\\.[a-zA-Z]{2,4}))[;:?\"!,.'>\\)]?(?=(?:\\s|$|>|\\.|,))",
    
    pipe = function(instance, 
                    removeUrl = TRUE,
                    URLPatterns = list(self$URLPattern, self$EmailPattern), 
                    namesURLPatterns = list("UrlPattern","EmailPattern")) {
   
      if (!"Instance" %in% class(instance)) {
        stop("[FindUrlPipe][pipe][Error]
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(removeUrl)) {
        stop("[FindUrlPipe][pipe][Error]
                Checking the type of the variable: removeUrl ", 
                  class(removeUrl))
      }

      if (!"list" %in% class(URLPatterns)) {
        stop("[FindUrlPipe][pipe][Error]
                Checking the type of the variable: URLPatterns ", 
                  class(URLPatterns))
      }

      if (!"list" %in% class(namesURLPatterns)) {
        stop("[FindUrlPipe][pipe][Error]
                 Checking the type of the variable: namesURLPatterns ", 
                   class(namesURLPatterns))
      }                
                          
      instance$addFlowPipes("FindUrlPipe")
      
      if (!instance$checkCompatibility("FindEmojiInStringBufferPipe", self$getAlwaysBeforeDeps())) {
        stop("[FindUrlPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      private$URLPatterns <- URLPatterns
      private$namesURLPatterns <- namesURLPatterns
      
      instance$getData() %>>%
        {lapply(private$URLPatterns, self$findUrl,.)} %>>%
          self$putNamesURLPattern() %>>%
            unlist() %>>%
              {instance$addProperties(.,super$getPropertyName())}

      if (removeUrl) {
        for (pattern in self$getURLPatterns()) {
          instance$getData() %>>%
            {self$removeUrl(pattern,.)} %>>%
              trim() %>>%
                instance$setData()
        }
      }
        
      if (is.na(instance$getData()) || 
          all(instance$getData() == "") || 
          is.null(instance$getData())) {
        
        message <- c( "The file: " , instance$getPath() , " has data empty on pipe Url")
        
        instance$addProperties(message, "reasonToInvalidate")   
        
        cat("[FindUrlPipe][pipe][Warning] ", message, " \n")

        instance$invalidate()
        
        return(instance)
      }
      
      return(instance)
    },
    
    findUrl = function(pattern, data) {
          
      if (!"character" %in% class(pattern)) {
        stop("[FindUrlPipe][findUrl][Error] 
             Checking the type of the variable: pattern ", 
             class(pattern))
      }               
      
      if (!"character" %in% class(data)) {
        stop("[FindUrlPipe][findUrl][Error] 
             Checking the type of the variable: data ", 
             class(data))
      }
      
      return(str_match_all(data,
                           regex(pattern,
                                 ignore_case = TRUE,
                                 multiline = TRUE))[[1]][,2] %>>% unique() %>>% unlist() )
    },  
    
    removeUrl = function(pattern, data) {
      
      if (!"character" %in% class(pattern)) {
        stop("[FindUrlPipe][removeUrl][Error] 
                Checking the type of the variable: pattern ", 
                  class(pattern))
      }               
        
      if (!"character" %in% class(data)) {
        stop("[FindUrlPipe][removeUrl][Error] 
                Checking the type of the variable: data ", 
                  class(data))
      }
        
      return(str_replace_all(data,
                              regex(pattern,
                                    ignore_case = TRUE,
                                    multiline = TRUE), " "))

    },
      
    putNamesURLPattern = function(resultOfURLPatterns) {
   
      if (!"list" %in% class(resultOfURLPatterns)) {
        stop("[FindUrlPipe][putNamesPattern][Error] 
                Checking the type of the variable: resultOfURLPatterns ", 
                  class(resultOfURLPatterns))
      }
        
      names(resultOfURLPatterns) <- self$getNamesURLPatterns() 
      
      return(resultOfURLPatterns)          
    },
      
    getURLPatterns = function() {

      return(private$URLPatterns)
    },
      
    getNamesURLPatterns = function() {

      return(private$namesURLPatterns)
    }
  ),  
  
  private = list(
    URLPatterns = list(),
    namesURLPatterns = list()
  )
)
