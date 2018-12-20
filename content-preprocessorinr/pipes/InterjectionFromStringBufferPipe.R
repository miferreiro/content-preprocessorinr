{
    InterjectionFromStringBufferPipe <- R6Class(
        "InterjectionFromStringBufferPipe",
        public = list(
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[InterjectionFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
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