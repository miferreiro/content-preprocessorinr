{
    File2StringBufferPipe <- R6Class(
        "File2StringBufferPipe",
       
        public = list(
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[File2StringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                instancia$obtainSource();
                return(instancia);
            },
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),  
        private = list(
            propertyName = "source"
        )
            
        
    )
}