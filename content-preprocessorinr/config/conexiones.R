{
    conexiones <- new.env();
    
    claves <- read.ini("content-preprocessorinr/config/configurations.ini")
    
    ######################################################################
    #####                   Conexiones a Twitter                    ######
    ######################################################################
    conexiones$conectadoTwitter <- FALSE;
    
    conexiones$startConectionWithTwitter = function(){
        if( !conexiones$conectadoTwitter ){
            
            reqURL <- "https://api.twitter.com/oauth/request_token";
            accessURL <- "https://api.twitter.com/oauth/access_token";
            authURL <- "https://api.twitter.com/oauth/authorize";
            options(httr_oauth_cache = T);
            consumer_key <- claves$twitter$ConsumerKey;
            consumer_secret <- claves$twitter$ConsumerSecret;
            access_token <- claves$twitter$AccessToken;
            access_secret <- claves$twitter$AccessTokenSecret;
            setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret);
            conexiones$conectadoTwitter <- TRUE;
            
            print("Twitter: Entra aqui una vez");
        }
    }
    
    conexiones$contadorDePeticionesTwitter <- 0;
    
    #Incrementa el numero de peticiones realizadas a twitter
    conexiones$incrementContadorDePeticionesTwitter = function(){
        conexiones$contadorDePeticionesTwitter = conexiones$contadorDePeticionesTwitter + 1
    }
    
    #Respeta el numero de peticiones de twitter
    conexiones$comprobacionDePeticionesTwitter = function(){
        if(conexiones$contadorDePeticionesTwitter >= 900) {
            Sys.sleep(900);
            conexiones$contadorDePeticionesTwitter = 0;
        }
    }
    
    ######################################################################
    #####                   Conexiones a Youtube                    ######
    ######################################################################
    
    conexiones$conectadoYoutube <- FALSE
    conexiones$startConectionWithYoutube = function(){
        if ( !conexiones$conectadoYoutube) {
         
            app_id <-  claves$youtube$app_id
            app_password <-  claves$youtube$app_password
            
            yt_oauth(app_id,app_password)
            
            conexiones$conectadoYoutube <- TRUE;
            print("Youtube: Entra aqui una vez");
        }
    }
    
    conexiones$contadorDePeticionesYoutube <- 0
    
    #Incrementa el numero de peticiones realizadas a youtube
    conexiones$incrementContadorDePeticionesYoutube = function(){
        conexiones$contadorDePeticionesYoutube = conexiones$contadorDePeticionesYoutube + 1
    }
    #Respeta el numero de peticiones de youtube
    conexiones$comprobacionDePeticionesYoutube = function(){
        if ( conexiones$contadorDePeticionesYoutube >= 900 ) {
            Sys.sleep(900);
            conexiones$contadorDePeticionesYoutube <- 0;
        }
    }
}
