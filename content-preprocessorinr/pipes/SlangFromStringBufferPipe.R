{
    SlangFromStringBufferPipe <- R6Class(
        "SlangFromStringBufferPipe",
        public = list(
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[SlangFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getData() %>>% 
                    self$replaceSlang() %>>% 
                        instancia$setData()
                # {instancia$addProperties(.,self$getPropertyName())}
            },
            
            replaceSlang = function(data){
                return(data %>>% replace_internet_slang())
            },
            
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),  
        private = list(
            propertyName = "Slang"
        )
    )
}