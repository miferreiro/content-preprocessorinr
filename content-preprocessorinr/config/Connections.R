#' @title Class to manage the connections with twitter and youtube
#' @description The tasks of the functions that the Connections class has are to
#' establish the connections and control the number of requests that have been made.
#' @docType class
#' @usage Connections$new(pathKeys)
#' @param pathKeys  (character) Path of the .ini file that contains the keys.
#' @details The way to indicate the keys of youtube and twitter has to be
#' through an .ini file that contains the following structure:
#'
#' [twitter]
#'
#' ConsumerKey=YourConsumerKey
#'
#' ConsumerSecret=YourConsumerSecret
#'
#' AccessToken=YourAccessToken
#'
#' AccessTokenSecret=YourAccessTokenSecret
#'
#' [youtube]
#'
#' app_id=YourAppId
#'
#' app_password=YourAppPassword
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{getTwitterToken}}{
#' Getter of twitterToken.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getTwitterToken()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of twitterToken.
#' }
#' }
#' }
#'
#' \item{\bold{startConnectionWithTwitter}}{
#' Function that establishes the connection to twitter. If the connection has
#' not been established, the keys necessary to make the connection are indicated
#' in the setup_twitter_oauth function. Then it is indicated that the connection
#' has been established.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{startConnectionWithTwitter()}
#' }
#' }
#' }
#'
#' \item{\bold{checkRequestToTwitter}}{
#' Function that controls the connection with twitter. If the limit of twitter
#' requests has been exceeded, 15 minutes are expected.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{checkRequestToTwitter()}
#' }
#' }
#' }
#'
#' \item{\bold{startConnectionWithYoutube}}{
#' Function that establishes the connection to youtube. If the connection has
#' not been established, the keys necessary to make the connection are indicated
#' in the yt_oauth function. Then it is indicated that the connection has been
#' established.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{startConnectionWithYoutube()}
#' }
#' }
#' }
#'
#' \item{\bold{addNumRequestToYoutube}}{
#' Function that increases in one the number of request to youtube.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{addNumRequestToYoutube()}
#' }
#' }
#' }
#'
#' \item{\bold{checkRequestToYoutube}}{
#' Function that controls the connection with youtube. If the limit of youtube
#' requests has been exceeded, 15 minutes are expected.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{checkRequestToYoutube()}
#' }
#' }
#' }
#'
#' \item{\bold{getNumRequestMaxToYoutube}}{
#' Getter of num max of from Youtube.
#' \itemize{
#' \item{\emph{Usage}}{
#'
#' \code{getNumRequestMaxToYoutube()}
#' }
#' \item{\emph{Value}}{
#'
#' Value of num max of from Youtube.
#' }
#' }
#' }
#' }
#'
#' @section Private fields:
#' \itemize{
#' \item{\bold{keys}}{
#'  (list) The keys of twitter and youtube.
#' }
#' \item{\bold{numRequestToYoutube}}{
#'  (numeric) Indicates the number of requests made.
#' }
#' \item{\bold{numRequestMaxToYoutube}}{
#'  (numeric) Indicates the maximum number of requests with Youtube.
#' }
#' \item{\bold{connectionWithYoutube}}{
#'  (logical) Indicates if the connection has been established with Youtube.
#' }
#' \item{\bold{connectionWithTwitter}}{
#'  (logical) Indicates if the connection has been established with Twitter.
#' }
#' \item{\bold{twitterToken}}{
#'  (Token) Token to establish the connection to twitter.
#' }
#' }
#'
#' @seealso \code{\link{ExtractorTwtid}}, \code{\link{ExtractorYtbid}}
#'
#' @import R6 tuber rtweet ini httr
#' @export Connections

Connections <- R6Class(
  
  "Connections",
  
  public = list(
    
    initialize = function(pathKeys) {

      if (!"character" %in% class(pathKeys)) {
        stop("[Connections][initialize][Error]
                Checking the type of the variable: pathKeys ",
                  class(pathKeys))
      }
      
      if (!"ini" %in% file_ext(pathKeys)) {
        stop("[Connections][initialize][Error]
                Checking the extension of the file: pathKeys ",
                  file_ext(pathKeys))
      }
      
      private$keys <- read.ini(pathKeys)
      
    },
    
    ######################################################################
    #####                    Twitter connections                    ######
    ######################################################################
    getTwitterToken = function() {

      return(private$twitterToken)
    },
    
    startConnectionWithTwitter = function() {

      if (!private$connectionWithTwitter) {
        
        tryCatch(
          {
            if (!file.exists(".rtweet_token.rds")) {
              private$twitterToken <- create_token(
                app = "my_twitter_research_app",
                consumer_key = private$keys$twitter$ConsumerKey,
                consumer_secret = private$keys$twitter$ConsumerSecret,
                access_token = private$keys$twitter$AcessToken,
                access_secret = private$keys$twitter$AccessTokenSecret,
                set_renv = T)
            } else {
              private$twitterToken <- readRDS(".rtweet_token.rds")
            }
          }
          ,
          
          error = function(e) {
            cat("[Connections][startConnectionWithTwitter][Error] Error on create_token \n")
          }
        )
        
        private$connectionWithTwitter <- TRUE
        
        cat("[Connections][startConectionWithTwitter][Info] Twitter: established connection\n")
      }
      return()
    },
    
    checkRequestToTwitter = function() {

      tryCatch(
      {
        if (rate_limit(token = self$getTwitterToken())[[3]][[54]] == 0) {
          cat("[Connections][checkRequestToTwitter][Info] ",
                paste(Sys.time()),"\n")
          
          cat("[Connections][checkRequestToTwitter][Info] ",
              "Waiting 15 min to be able to make new requests from twitter...\n")
          
          Sys.sleep(900)
        } else{
          cat("[Connections][checkRequestToTwitter][Info] ",
                "There are ", rate_limit(token = self$getTwitterToken())[[3]][[54]], 
                  " twitter requests to be consumed\n")
        }
      }
      ,
        warning = function(w) {
          cat("[Connections][checkRequestToTwitter][Warning]
                    ", paste(w), " \n")
          
          cat("[Connections][checkRequestToTwitter][Info] ",
              paste(Sys.time()),"\n")
          
          cat("[Connections][checkRequestToTwitter][Info] ",
              "Waiting 15 min to be able to make new requests from twitter...\n")
          
          Sys.sleep(900)
        }
      )
      return()
    },
    ######################################################################
    #####                   Youtube connections                     ######
    ######################################################################
    startConnectionWithYoutube = function() {

      if (!private$connectionWithYoutube) {
        yt_oauth(private$keys$youtube$app_id,
                 private$keys$youtube$app_password)
        
        private$connectionWithYoutube <- TRUE

        
        cat("[Connections][startConnectionWithYoutube][Info] Youtube: established connection\n")
      }
      
      return()
      
    },
    
    addNumRequestToYoutube = function() {

      private$numRequestToYoutube <- private$numRequestToYoutube + 1
      return()
    },
    
    checkRequestToYoutube = function() {

      if (private$numRequestToYoutube >= self$getNumRequestMaxToYoutube()) {
        cat("[Connections][checkRequestToYoutube][Info] ",
            "Waiting 15 min to be able to make new requests from youtube...\n")
        Sys.sleep(900)
        private$numRequestToYoutube <- 0
      }
    },
    
    getNumRequestMaxToYoutube = function() {

      return(private$numRequestMaxToYoutube)
    }
    
  ),
  
  private = list(
    keys = "",
    numRequestToYoutube = 0,
    numRequestMaxToYoutube = 900,
    connectionWithYoutube = FALSE,
    connectionWithTwitter = FALSE,
    twitterToken = NULL
  )
)