{
    TeeCSVFromStringBufferPipe <- R6Class(
        "TeeCSVFromStringBufferPipe",
        public = list(
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[TeeCSVFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
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