{
    connections <- new.env();
    
    keys <- read.ini("content-preprocessorinr/config/configurations.ini")
    
    ######################################################################
    #####                   Conexiones a Twitter                    ######
    ######################################################################
    connections$connectionWithTwitter <- FALSE;
    
    connections$startConectionWithTwitter = function(){
        if ( !connections$connectionWithTwitter ){
            
            # reqURL <- "https://api.twitter.com/oauth/request_token";
            # accessURL <- "https://api.twitter.com/oauth/access_token";
            # authURL <- "https://api.twitter.com/oauth/authorize";
            options(httr_oauth_cache = T);
            consumer_key <- keys$twitter$ConsumerKey;
            consumer_secret <- keys$twitter$ConsumerSecret;
            access_token <- keys$twitter$AccessToken;
            access_secret <- keys$twitter$AccessTokenSecret;
            setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret);
            connections$connectionWithTwitter <- TRUE;
            
           # cat("Twitter: Entra aqui una vez\n");
        }
    }
    
    connections$numRequestToTwitter <- 0;
    
    #Incrementa el numero de peticiones realizadas a twitter
    connections$addNumRequestToTwitter = function(){
        connections$numRequestToTwitter = connections$numRequestToTwitter + 1
    }
    
    #Respeta el numero de peticiones de twitter
    connections$checkRequestToTwitter = function(){
        if (connections$numRequestToTwitter >= 900) {
            cat("Toca esperar 15 min");
            Sys.sleep(900);
            connections$numRequestToTwitter = 0;
        }
    }
    
    ######################################################################
    #####                   Conexiones a Youtube                    ######
    ######################################################################
    
    connections$connectionWithYoutube <- FALSE
    connections$startConectionWithYoutube = function(){
        if ( !connections$connectionWithYoutube) {
         
            app_id <-  keys$youtube$app_id
            app_password <-  keys$youtube$app_password
            
            yt_oauth(app_id,app_password)
            
            connections$connectionWithYoutube <- TRUE;
            #cat("Youtube: Entra aqui una vez\n");
        }
    }
    
    connections$numRequestToYoutube <- 0
    
    #Incrementa el numero de peticiones realizadas a youtube
    connections$addNumRequestToYoutube = function(){
        connections$numRequestToYoutube = connections$numRequestToYoutube + 1
    }
    #Respeta el numero de peticiones de youtube
    connections$checkRequestToYoutube = function(){
        if ( connections$numRequestToYoutube >= 900 ) {
            Sys.sleep(900);
            connections$numRequestToYoutube <- 0;
        }
    }
}