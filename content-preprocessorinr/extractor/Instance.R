#' @title Super class that handles the general functionalities of the management
#' of the instances
#' @description The tasks of the functions, that the Instance class has, are to
#' handle the variables associated with an instance.
#' @docType class
#' @usage Instance$new(path)
#' @param path  (character) Path of the file.
#' @details Building...
#' @section Methods:
#' \itemize{
#' \item{\bold{obtainDate}}{
#' Abstract function to obtain the date that has to be implemented by the
#' classes that inherit from instance.
#' }
#' \item{\bold{obtainSource}}{
#' Abstract function to obtain the source that has to be implemented by the
#' classes that inherit from instance.
#' }
#' \item{\bold{getDate}}{
#' Getter of date.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getDate()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of date.
#' }
#' }
#' }
#'
#' \item{\bold{setDate}}{
#' Setter of date.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setDate(date)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{date}}{
#' (character) The new value of date.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getSource}}{
#' Getter of source.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getSource()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of source.
#' }
#' }
#' }
#'
#' \item{\bold{setSource}}{
#' Setter of source.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setSource(source)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{source}}{
#' (character) The new value of source.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getPath}}{
#' Getter of path.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getPath()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of path.
#' }
#' }
#' }
#'
#' \item{\bold{getProperties}}{
#' Getter of properties.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getProperties()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of properties.
#' }
#' }
#' }
#'
#' \item{\bold{setProperties}}{
#' Setter of properties.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setProperties(properties)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{properties}}{
#' (list) The new value of properties.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{addProperties}}{
#' Add a property to the list of properties.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{addProperties(propertyValue, propertyName)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{propertyValue}}{
#' () The value of the new property.
#' }
#' \item{\strong{propertyName}}{
#' (character) The name of the new property.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getSpecificProperty}}{
#' Obtains a specific property.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getSpecificProperty(propertyName)}
#' }
#' \item{\emph{Value}}{
#'
#' The value of the specific property.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{propertyName}}{
#' (character) The name of the property to obtain.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{isSpecificProperty}}{
#' Obtains if exists a specific property.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{isSpecificProperty(propertyName)}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE or FALSE depending on whether the property is on the list of properties.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{propertyName}}{
#' (character) The name of the property to check.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{setSpecificProperty}}{
#' Changes the value of the one property.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{setSpecificProperty(propertyName, propertyValue)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{propertyName}}{
#' () The new value of the property.
#' }
#' \item{\strong{propertyValue}}{
#' (character) The name of the  property.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getNamesOfProperties}}{
#' Getter of the names of properties.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getNamesOfProperties()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of the names of properties.
#' }
#' }
#' }
#'
#' \item{\bold{isInstanceValid}}{
#' Obtains if the Instance is valid.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{isInstanceValid()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of isValid.
#' }
#' }
#' }
#'
#' \item{\bold{invalidate}}{
#' Sets the instance in the invalid state.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{invalidate()}
#' }
#' }
#' }
#'
#' \item{\bold{getFlowPipes}}{
#' Get the pipe flow list.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getNamesOfProperties()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of pipe flow list.
#' }
#' }
#' }
#'
#' \item{\bold{addFlowPipes}}{
#' Added the name of the pipe to the list that keeps track of the flow of pipes
#' that the instance has gone through.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{addFlowPipes(namePipe)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{namePipe}}{
#' (character) Pipe name to be introduced into the flow.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getBanPipes}}{
#' Get the pipe flow array.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getNamesOfProperties()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of pipe ban array.
#' }
#' }
#' }
#'
#' \item{\bold{addBanPipes}}{
#' Added the name of the pipe to the array that keeps track pipes that can not
#' be run after.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{addBanPipes(namePipe)}
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{namePipe}}{
#' (character) Pipe name to be introduced into the ban array.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{checkCompatibility}}{
#' Check compability between pipes.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{checkCompatibility(namePipe, alwaysBefore)}
#' }
#' \item{\emph{Value}}{
#'
#' TRUE/FALSE depends if the compability between pipes is correctly or not.
#' }
#' \item{\emph{Arguments}}{
#' \itemize{
#' \item{\strong{namePipe}}{
#' (character) Name of the pipe to check the compatibility.
#' }
#' \item{\strong{alwaysBefore}}{
#' (list) Pipes that the instance had to go through.
#' }
#' }
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{date}}{
#'  (character) The date on which the source was generated or sent.
#' }
#' \item{\bold{source}}{
#'  (character) The text of the file without modifications.
#' }
#' \item{\bold{path}}{
#'  (character) Identifier of the instance, in this case it will be the path of
#'  the file from which the properties are extracted.
#' }
#' \item{\bold{data}}{
#'  (character) The text of the file with modifications.
#' }
#' \item{\bold{properties}}{
#'  (list) Contains a list of properties extracted from the text that is being
#'  processed.
#' }
#' \item{\bold{isValid}}{
#'  (logical) Indicates if the instance is valid or not.
#' }
#' \item{\bold{flowPipes}}{
#'  (list) The list contains the pipes that the instance has passed through.
#' }
#' \item{\bold{banPipes}}{
#'  (array) The list contains the pipes that can not be executed from that moment.
#' }
#' }
#'
#' @import R6  rlist pipeR
#' @export Instance

Instance <- R6Class(
  
  "Instance",
  
  public = list(
    
    initialize = function(path) {

      if (!"character" %in% class(path)) {
        stop("[Instance][initialize][Error]
                Checking the type of the variable: path ",
                  class(path))
      }
      
      private$path <- path
      
      self$addProperties(self$getPath(), "Initial_path")
      
    },
    
    obtainDate = function() {

      stop("[Instance][obtainDate][Error]
              I'm an abstract interface method")
    },
    
    obtainSource = function() {

      stop("[Instance][obtainSource][Error]
              I'm an abstract interface method")
    },
    
    getDate = function() {

      return(private$date)
    },
    
    getSource = function() {

      return(private$source)
    },
    
    getPath = function() {

      return(private$path)
    },
    
    getData = function() {

      return(private$data)
    },
    
    getProperties = function() {

      return(private$properties)
    },
    
    setSource = function(source) {

      if (!"character" %in% class(source)) {
        stop("[Instance][setSource][Error]
                Checking the type of the variable: source ",
                  class(source))
      }
      
      private$source <- source
      
      return()
    },
    
    setDate = function(date) {

      if (!"character" %in% class(date)) {
        stop("[Instance][setDate][Error]
                Checking the type of the variable: date ",
                  class(date))
      }
      
      private$date <- date
      
      return()
    },
    
    setProperties = function(properties) {

      if (!"list" %in% class(properties)) {
        stop("[Instance][setProperties][Error]
                Checking the type of the variable: properties ",
                  class(properties))
      }
      
      private$properties <- properties
      
      return()
    },
    
    addProperties = function(propertyValue, propertyName) {

      if (!"character" %in% class(propertyName)) {
        stop("[Instance][addProperties][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      private$properties <- list.append(self$getProperties(), propertyValue)
      
      names(private$properties)[length(self$getProperties())] <- propertyName
      
      return()
    },
    
    getSpecificProperty = function(propertyName) {

      if (!"character" %in% class(propertyName)) {
        stop("[Instance][getSpecificProperty][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      return(self$getProperties()[[propertyName]])
    },
    
    isSpecificProperty = function(propertyName) {

      return(propertyName %in% self$getNamesOfProperties())
    },
    
    setSpecificProperty = function(propertyName, propertyValue) {

      if (!"character" %in% class(propertyName)) {
        stop("[Instance][setSpecificProperty][Error]
                Checking the type of the variable: propertyName ",
                  class(propertyName))
      }
      
      private$properties[[nombrePropiedad]] <- propertyValue
      
      return()
    },
    
    getNamesOfProperties = function() {

      return(self$getProperties() %>>% names())
    },
    
    setData = function(data) {

      if (!"character" %in% class(data)) {
        stop("[Instance][setData][Error]
                Checking the type of the variable: data ",
                  class(data))
      }
      
      private$data <- data
      
      return()
    },
    
    isInstanceValid = function() {

      return(private$isValid)
    },
    
    invalidate = function() {

      private$isValid <- FALSE
      
      return()
    },
    
    getFlowPipes = function() {

      return(private$flowPipes)
    },
    
    addFlowPipes = function(namePipe) {
   
      if (!"character" %in% class(namePipe)) {
        stop("[Instance][addFlowPipes][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }
      
      private$flowPipes <- list.append(private$flowPipes, namePipe)
      
      return()
    },
    
    getBanPipes = function() {

      return(private$banPipes)
    },
    
    addBanPipes = function(namePipe) {

      if (!"character" %in% class(namePipe) & !is.null(namePipe)) {
        stop("[Instance][addBanPipes][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }
      
      if (!is.null(namePipe)) {
        private$banPipes <- c(private$banPipes, namePipe)
      }
      
      return()
    },
    
    checkCompatibility = function(namePipe, alwaysBefore) {

      if (!"character" %in% class(namePipe)) {
        stop("[Instance][checkCompatibility][Error]
                Checking the type of the variable: namePipe ",
                  class(namePipe))
      }
      
      if (!"list" %in% class(alwaysBefore)) {
        stop("[Instance][checkCompatibility][Error]
                Checking the type of the variable: alwaysBefore ",
                  class(alwaysBefore))
      }
      
      for (depsB in alwaysBefore) {
        
        if (!depsB %in% self$getFlowPipes()) {
          return(FALSE)
        }
      }
      
      if (namePipe %in% self$getBanPipes()) {
        return(FALSE)
      }
      
      return(TRUE)
    }
  ),
  
  private = list(
    date = "",
    source = "",
    path = "",
    data = "",
    properties = list(),
    isValid = TRUE,
    flowPipes = list() ,
    banPipes = c()
  )
)