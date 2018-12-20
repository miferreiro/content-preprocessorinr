{
    StringBufferToLowerCasePipe <- R6Class(
        "StringBufferToLowerCasePipe",
        
        public = list(
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[StringBufferToLowerCasePipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getData() %>>% 
                    self$toLowerCase() %>>%
                    {instancia$setData(.)}
                
                return(instancia);
            },
            
            toLowerCase = function(data){
                return(data %>>% tolower());
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