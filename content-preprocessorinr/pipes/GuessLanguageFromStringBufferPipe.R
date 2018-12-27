{
    GuessLanguageFromStringBufferPipe <- R6Class(
        
        "GuessLanguageFromStringBufferPipe",
        
        public = list(

            pipe = function(instance,languageTwitter = TRUE){
                
                
                if (!"ExtractorSource" %in% class(instance)) {
                    stop("[GuessLanguageFromStringBufferPipe][pipe][Error] 
                         Checking the type of the variable: instance ", class(instance));
                }
                
                if (!languageTwitter && !instance$getSpecificProperties("extenxion") %in% "twtid") {
                
                    instance$getData() %>>% 
                        self$getLanguage() %>>%
                            {instance$addProperties(.,self$getPropertyName())}
      
                    instance$getData() %>>% 
                        self$getLanguageScore() %>>%
                            {instance$addProperties(.,"Language score")}
                    
                    instance$getData() %>>% 
                        self$getLanguagePercent() %>>%
                            {instance$addProperties(.,"Language percent")}
                }else{
                   
                    #Completar aqui con el lang de twitter (No he encontrado la funcion que me devuelva el lang)
                    
                }
                return(instance);
            },
            
            getLanguage = function(data) {
                
                if (!"character" %in% class(data)) {
                    stop("[GuessLanguageFromStringBufferPipe][getLanguage][Error] 
                         Checking the type of the variable: data ", class(data));
                }
                
                return(detectLanguage(data)[[1]])
            },
            
            getLanguageScore = function(data){

                if (!"character" %in% class(data)) {
                    stop("[GuessLanguageFromStringBufferPipe][getLanguageScore][Error] 
                         Checking the type of the variable: data ", class(data));
                }
                
                return(detectLanguage(data)[[7]])  
            },
            
            getLanguagePercent = function(data){

                if (!"character" %in% class(data)) {
                    stop("[GuessLanguageFromStringBufferPipe][getLanguagePercent][Error] 
                         Checking the type of the variable: data ", class(data));
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