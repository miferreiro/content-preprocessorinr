#' @title Class to build Instance types
#' @description This class uses the standard factory method because it is
#' responsible for choosing the type of instances that will be created from the
#' files indicated in the path.
#' @docType class
#' @usage InstanceFactory$new()
#' @section Methods:
#' \itemize{
#' \item{\bold{createInstance}}{
#' Function that builds instances from the path it receives. Depending on the
#' extension of the file indicated in the path, an instance type or other is
#' created.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{createInstance(path)}
#' }
#' \item{\emph{Value}}{
#'
#' The corresponding object according to the file extension.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{path}}{
#' (character) Path of the file to create an instance.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{Instance}}, \code{\link{ExtractorEml}},
#' \code{\link{ExtractorSms}}, \code{\link{ExtractorTtwt}},
#' \code{\link{ExtractorTwtid}}, \code{\link{ExtractorTytb}},
#' \code{\link{ExtractorWarc}}, \code{\link{ExtractorYtbid}}
#'
#' @import R6  tools
#' @export InstanceFactory

InstanceFactory <- R6Class(
    
  "InstanceFactory",
    
  public = list(
        
    initialize = function() {
            
    },
        
    createInstance = function(path) {
          
      if (!"character" %in% class(path)) {
        stop("[InstanceFactory][createInstance][Error] 
                Checking the type of the variable: path ", 
                  class(path))
      }
        
      switch(file_ext(path),
       `eml` =  return(ExtractorEml$new(path)),
       `tsms` = return(ExtractorSms$new(path)),
       `twtid` = return(ExtractorTwtid$new(path)),
       `ttwt` = return(ExtractorTtwt$new(path)),
       `warc` = return(ExtractorWarc$new(path)),
       `tytb` = return(ExtractorTytb$new(path)),
       `ytbid` = return(ExtractorYtbid$new(path))
      )
      
      return()
    }
  )
)
