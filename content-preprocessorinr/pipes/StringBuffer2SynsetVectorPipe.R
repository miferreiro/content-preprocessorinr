{
    StringBuffer2SynsetVectorPipe <- R6Class(
        "StringBuffer2SynsetVectorPipe",
        public = list(
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[StringBuffer2SynsetVectorPipe][Error] Comprobacion del tipo de la variable instancia");
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