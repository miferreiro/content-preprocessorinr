{
    StripHTMLFromStringBufferPipe <- R6Class(
        "StripHTMLFromStringBufferPipe",
        
        public = list(
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[StripHTMLFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }

                instancia$getSource() %>>% 
                    self$getDataWithOutHtml() %>>%
                    {instancia$setData(.)}
                
                return(instancia);
            },
            
            getDataWithOutHtml = function(source){
               
                return( source %>>% replace_html() )
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