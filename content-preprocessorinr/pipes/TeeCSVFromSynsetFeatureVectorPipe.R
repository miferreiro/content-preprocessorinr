#' @title Class to complete the csv with the preprocessed instance and synsets
#' @description Complete the csv with the preprocessed instance and synsets.
#' @docType class
#' @usage TeeCSVFromSynsetFeatureVectorPipe$new(propertyName = "",
#'                                       alwaysBeforeDeps = list(),
#'                                       notAfterDeps = list())
#' @param propertyName  (character) Name of the property associated with the pipe.
#' @param alwaysBeforeDeps (list) The dependences alwaysBefore (pipes that must
#' be executed before this one).
#' @param notAfterDeps (list) The dependences notAfter (pipes that cannot be
#' executed after this one).
#' @details It is necessary to identify the properties associated with the
#' synsets that the instance will have, so as not to include them in the
#' data.frame.
#'
#' @section Inherit:
#' This class inherit from \code{\link{PipeGeneric}} and implements the
#' \code{pipe} abstract function.
#' @section Methods:
#' \itemize{
#' \item{\bold{pipe}}{
#' Function that complete the csv with the preprocessed instance and synsets.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{pipe(instance, withData = TRUE, withSource = TRUE,
#' listPropertySynsets = c("synsetVector", "synsetFeatureVector"),
#' outPutPath = "dataFrameAllSynsets.csv")}
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
#' \item{\strong{withData}}{
#' (logical) Indicate if the data is added to csv.
#' }
#' \item{\strong{withSource}}{
#' (logical) Indicate if the source is added to csv.
#' }
#' \item{\strong{listPropertySynsets}}{
#' (character) vector indicating properties related to synsets.
#' }
#' \item{\strong{outPutPath}}{
#' (character) name of the csv to store synsets and properties of the instance.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{PipeGeneric}}, \code{\link{Instance}}
#'
#' @import R6
#' @export TeeCSVFromSynsetFeatureVectorPipe

TeeCSVFromSynsetFeatureVectorPipe <- R6Class(
  
  "TeeCSVFromSynsetFeatureVectorPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {

      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance, withData = TRUE, withSource = TRUE, 
                    listPropertySynsets = c("synsetVector", "synsetFeatureVector"),
                    outPutPath = "dataFrameAllSynsets.csv") {
          
      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(withSource)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: withSource ", 
                  class(withSource))
      }
      
      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: withData ", 
                  class(withData))
      }
      
      if (!"character" %in% class(listPropertySynsets)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: listPropertySynsets ", 
                  class(listPropertySynsets))
      }      
      
      if (!"character" %in% class(outPutPath)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: outPutPath ", 
                  class(outPutPath))
      }
      
      if (!"csv" %in% file_ext(outPutPath)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error]
                Checking the extension of the file: outPutPath ",
                  file_ext(outPutPath))
      }
      
      instance$addFlowPipes("TeeCSVFromSynsetFeatureVectorPipe")
      
      if (!instance$checkCompatibility("TeeCSVFromSynsetFeatureVectorPipe", self$getAlwaysBeforeDeps())) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      if (!instance$isInstanceValid()) {
        return(instance)
      }
      
      if (file.exists(outPutPath)) {
        dataFrameAllSynsets <- read.csv(file = outPutPath, header = TRUE, 
                                        sep = ";", dec = ".", fill = FALSE, stringsAsFactors = FALSE)
      } else {
        dataFrameAllSynsets <- data.frame()
      }
      
      pos <- dim(dataFrameAllSynsets)[1] + 1
      
      dataFrameAllSynsets[pos, "path"] <- instance$getPath()
      
      if (withData) {
        dataFrameAllSynsets[pos, "data"] <- instance$getData()
      }
      
      if (withSource) {
        dataFrameAllSynsets[pos, "source"] <- as.character(paste0(unlist(instance$getSource())))
      }
      
      dataFrameAllSynsets[pos, "date"] <- instance$getDate()
      
      namesPropertiesList <- as.list(instance$getNamesOfProperties())
      names(namesPropertiesList) <- instance$getNamesOfProperties()
      
      for (name in list.remove(namesPropertiesList, listPropertySynsets)) { 
        dataFrameAllSynsets[pos, name] <- 
          paste0(unlist(instance$getSpecificProperty(name)), collapse = "|")
      }
      
      synsets <- instance$getSpecificProperty("synsetFeatureVector")
      
      synsetFeature <- synsets$getSynsetsFeature()
      
      for (synset in names(synsetFeature)) {
        dataFrameAllSynsets[pos, synset] <- synsetFeature[[synset]]
      }
    
      write.table(x = dataFrameAll, 
                  file = outPutPath, 
                  sep = ";", 
                  dec = ".",
                  quote = T,
                  col.names = TRUE, 
                  row.names = FALSE,
                  qmethod = c("double"),
                  fileEncoding = "UTF-8")
      
      return(instance)
    }
  )
)