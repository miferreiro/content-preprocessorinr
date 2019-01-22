#Class to manage the connections with twitter and youtube
#
#The tasks of the functions that the Connections class has are to establish 
#the connections and control the number of requests that have been made.
#
#Variables:
#
#keys: (list) has the keys of twitter and youtube
#numRequestToTwitter: (numeric)  Indicates the number of requests made
#numRequestMaxToTwitter: (numeric) Indicates the maximum 
#                                           number of requests with Twitter
#numRequestToYoutube: (numeric) Indicates the number of requests made
#numRequestMaxToYoutube: (numeric) Indicates the maximum 
#                                           number of requests with Youtube
#connectionWithYoutube:  (logical) Indicates if the connection 
#                                           has been established with Youtube
#connectionWithTwitter:  (logical) Indicates if the connection 
#                                           has been established with Twitter
Connections <- R6Class(
  
  "Connections",
    
  public = list(

    initialize = function(pathKeys = "content-preprocessorinr/config/configurations.ini") {
      #
      #Class constructor 
      #
      #This constructor initialize the variable of keys. This variable 
      #contains the keys' list of the connections that are stored 
      #in the file indicated in the variable
      #
      #Args: 
      #   pathKeys: (character) Path of the .ini file that contains the keys
      #
      #Returns: 
      #   null
      #               
      if (!"character" %in% class(pathKeys)) {
        stop("[Connections][initialize][Error] 
             Checking the type of the variable: pathKeys ", class(pathKeys))
      }
      
      private$keys <- read.ini(pathKeys)
      
    },
      
    ######################################################################
    #####                   Conexiones a Twitter                    ######
    ######################################################################
    
    startConectionWithTwitter = function() {
      #
      #Function that establishes the connection to twitter
      #
      #If the connection has not been established, the keys necessary to make 
      #the connection are indicated in the setup_twitter_oauth function. 
      #Then it is indicated that the connection has been established.
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #   
      if (!private$connectionWithTwitter) {
        
        tryCatch(create_token(
          app = "my_twitter_research_app",
          consumer_key = private$keys$twitter$ConsumerKey,
          consumer_secret = private$keys$twitter$ConsumerSecret,
          access_token = private$keys$twitter$AcessToken,
          access_secret = private$keys$twitter$AccessTokenSecret, set_renv = T) ,
          
          error = function(e){
            print("Error en create_token")
          }
        )
                      
        private$connectionWithTwitter <- TRUE
        
        cat("Twitter: Entra aqui una vez\n")
      }
      return()
    },
 
    addNumRequestToTwitter = function() {
      #
      #Function that increases in one the number of request to twitter.
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #  
      private$numRequestToTwitter <- private$numRequestToTwitter + 1
      return()
    },

    checkRequestToTwitter = function() {
      #
      #Function that controls the connection with twitter.
      #
      #If the limit of twitter requests has been exceeded, 
      #15 minutes are expected
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #   
      if (private$numRequestToTwitter >= self$getNumRequestMaxToTwitter()) {
        cat("Toca esperar 15 min")
        Sys.sleep(900)
        private$numRequestToTwitter <- 0
      }
      
      return()
    },
    ######################################################################
    #####                   Conexiones a Youtube                    ######
    ######################################################################
    startConectionWithYoutube = function() {
      #
      #Function that establishes the connection to youtube
      #
      #If the connection has not been established, the keys necessary to make 
      #the connection are indicated in the yt_oauth function. 
      #Then it is indicated that the connection has been established.
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #   
      if (!private$connectionWithYoutube) {

        yt_oauth(private$keys$youtube$app_id, 
                 private$keys$youtube$app_password)
         
        private$connectionWithYoutube <- TRUE
        
        cat("Youtube: Entra aqui una vez\n")
      }
      
      return();
    },

    addNumRequestToYoutube = function() {
      #
      #Function that increases in one the number of request to youtube
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #   
      private$numRequestToYoutube <- private$numRequestToYoutube + 1
      return()
    },

    checkRequestToYoutube = function() {
      #
      #Function that controls the connection with youtube
      #
      #If the limit of youtube requests has been exceeded, 
      #15 minutes are expected
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   null
      #   
      if (private$numRequestToYoutube >= self$getNumRequestMaxToYoutube()) {
        cat("Toca esperar 15 min")
        Sys.sleep(900)
        private$numRequestToYoutube <- 0
      }
    },
    
    getNumRequestMaxToTwitter = function() {
      #
      #Getter of numRequestMaxTwitter variable
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   value of numRequestMaxTwitter variable
      #   
      return(private$numRequestMaxToTwitter)
    },
     
    getNumRequestMaxToYoutube = function() {
      #
      #Getter of numRequestMaxYoutube variable
      #
      #Args: 
      #   null
      #
      #Returns: 
      #   value of numRequestMaxYoutube variable
      #    
      return(private$numRequestMaxToYoutube)
    },
    
    ######################################################################
    #####                   Creado csv                              ######
    ######################################################################        
    checkFirstCsv = function(){
      return(private$CsvStatus);
    },
    
    setCsvStatus = function(status){
      private$CsvStatus = status
    }

  ),
  
  private = list(
    keys = "",
    numRequestToTwitter = 0,
    numRequestMaxToTwitter = 900,
    numRequestToYoutube = 0,
    numRequestMaxToYoutube = 900,
    connectionWithYoutube = FALSE,
    connectionWithTwitter = FALSE,
    CsvStatus = FALSE
  )
)