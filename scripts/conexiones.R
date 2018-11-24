{
    conexiones <- new.env();
    conexiones$conectado <- FALSE
    conexiones$startConectionWithTwitter = function(){
        if(!conexiones$conectado){
            reqURL <- "https://api.twitter.com/oauth/request_token"; 
            accessURL <- "https://api.twitter.com/oauth/access_token"; 
            authURL <- "https://api.twitter.com/oauth/authorize"; 
            options(httr_oauth_cache = T); 
            consumer_key <- "CONSUMER_KEY"; 
            consumer_secret <- "CONSUMER_SECRET"; 
            access_token <- "ACCESS_TOKEN"; 
            access_secret <- "ACCESS_SECRET"; 
            setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret); 
            conexiones$conectadoTwitter <- TRUE; 
             
            print("Twitter: Entra aqui una vez"); 
        }
    }
    conexiones$contadorDePeticiones <- 0
    
    #Incrementa el numero de peticiones realizadas a twitter
    conexiones$incrementContadorDePeticiones = function(){
        conexiones$contadorDePeticiones = conexiones$contadorDePeticiones + 1
    }
    #Respeta el numero de peticiones
    conexiones$comprobacionDePeticiones = function(){
        if(conexiones$contadorDePeticiones >= 900){
            Sys.sleep(900);
            conexiones$contadorDePeticiones = 0;
        }
        
    }
    
    # conexiones$startConectionWithYoutube = function(){
    #     
    #     yt_oauth(,)
    # }
}
