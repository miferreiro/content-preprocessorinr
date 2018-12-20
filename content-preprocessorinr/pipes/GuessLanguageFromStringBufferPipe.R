{
    GuessLanguageFromStringBufferPipe <- R6Class(
        "GuessLanguageFromStringBufferPipe",
        
        public = list(

            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[GuessLanguageFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getData() %>>% 
                    self$getLanguage() %>>%
                    {instancia$addProperties(.,self$getPropertyName())}
  
                instancia$getData() %>>% 
                    self$getLanguageScore() %>>%
                    {instancia$addProperties(.,"Language score")}
                
                instancia$getData() %>>% 
                    self$getLanguagePercent() %>>%
                    {instancia$addProperties(.,"Language percent")}
                
                return(instancia);
            },
            
            getLanguage = function(data) {
                if (!"character" %in% class(data)){
                    stop("[GuessLanguageFromStringBufferPipe][Error] Comprobacion del tipo de la variable data");
                }
                return(detectLanguage(data)[[1]])
            },
            getLanguageScore = function(data){
                if (!"character" %in% class(data)){
                    stop("[GuessLanguageFromStringBufferPipe][Error] Comprobacion del tipo de la variable data");
                }
                return(detectLanguage(data)[[7]])  
            },
            getLanguagePercent = function(data){
                if (!"character" %in% class(data)){
                    stop("[GuessLanguageFromStringBufferPipe][Error] Comprobacion del tipo de la variable data");
                }
                return(detectLanguage(data)[[10]])  
            },  
            
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),  
        private = list(
            propertyName = "language"
        )
    )
}