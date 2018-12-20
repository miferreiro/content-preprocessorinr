{
    AbbreviationFromStringBufferPipe <- R6Class(
        "AbbreviationFromStringBufferPipe",
        public = list(
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[AbbreviationFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                return(instancia);
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