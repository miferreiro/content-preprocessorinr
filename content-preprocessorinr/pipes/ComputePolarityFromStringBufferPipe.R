{
    ComputePolarityFromStringBufferPipe <- R6Class(
        "ComputePolarityFromStringBufferPipe",
        public = list(
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[ComputePolarityFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
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