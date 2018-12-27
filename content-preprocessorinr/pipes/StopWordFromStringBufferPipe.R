{
    StopWordFromStringBufferPipe <- R6Class(
        "StopWordFromStringBufferPipe",
        public = list(
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[StopWordFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getData() %>>% 
                    self$stopWords() %>>%
                        {instancia$setData(.)}
                
                return(instancia);
            },
            
            stopWords = function(data){
                return(data )
            },
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),  
        private = list(
            propertyName = ""
        )
    )
}