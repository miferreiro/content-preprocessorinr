Connections <- R6Class(
    "Connections",
    public = list(
        initialize = function() {
            private$keys <- read.ini("content-preprocessorinr/config/configurations.ini");
        },
        ######################################################################
        #####                   Conexiones a Twitter                    ######
        ######################################################################
        startConectionWithTwitter = function(){
            if ( !private$connectionWithTwitter ) {
                
                options(httr_oauth_cache = T);
                setup_twitter_oauth(private$keys$twitter$ConsumerKey, 
                                    private$keys$twitter$ConsumerSecret, 
                                    private$keys$twitter$AccessToken, 
                                    private$keys$twitter$AccessTokenSecret);
                private$connectionWithTwitter <- TRUE;
                
                cat("Twitter: Entra aqui una vez\n");
            }
        },
        #Incrementa el numero de peticiones realizadas a twitter
        addNumRequestToTwitter = function(){
            private$numRequestToTwitter = private$numRequestToTwitter + 1
        },
        #Respeta el numero de peticiones de twitter
        checkRequestToTwitter = function(){
            if (private$numRequestToTwitter >= 900) {
                cat("Toca esperar 15 min");
                Sys.sleep(900);
                private$numRequestToTwitter = 0;
            }
        },
        ######################################################################
        #####                   Conexiones a Youtube                    ######
        ######################################################################
        
        startConectionWithYoutube = function(){
            if ( !private$connectionWithYoutube) {

                yt_oauth( private$keys$youtube$app_id,private$keys$youtube$app_password)
                
                private$connectionWithYoutube <- TRUE;
                cat("Youtube: Entra aqui una vez\n");
            }
        },
        
        #Incrementa el numero de peticiones realizadas a youtube
        addNumRequestToYoutube = function(){
            private$numRequestToYoutube = private$numRequestToYoutube + 1
        },
        
        #Respeta el numero de peticiones de youtube
        checkRequestToYoutube = function(){
            if ( private$numRequestToYoutube >= 900 ) {
                Sys.sleep(900);
                private$numRequestToYoutube <- 0;
            }
        }
        
    ),
    private = list(
        keys = "",
        numRequestToTwitter = 0,
        numRequestToYoutube = 0,
        connectionWithYoutube = FALSE,
        connectionWithTwitter = FALSE

    )
)