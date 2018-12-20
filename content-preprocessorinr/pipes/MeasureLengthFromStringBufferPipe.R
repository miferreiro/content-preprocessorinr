{
    MeasureLengthFromStringBufferPipe <- R6Class(
        "MeasureLengthFromStringBufferPipe",
        
        public = list(
            pipe = function(instancia, propertyName = self$getPropertyName() ){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[MeasureLengthFromStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                if (!"character" %in% class(propertyName)) {
                    stop("[MeasureLengthFromStringBufferPipe][Error] Comprobacion del tipo de la variable propertyName");
                }
                
                instancia$getSource() %>>% 
                    self$getLength() %>>%
                        {instancia$addProperties(.,propertyName)}
                
                return(instancia);
            },
            
            getLength = function(data, nchar_conf = TRUE) {
                if (!"character" %in% class(data)) {
                    stop("[MeasureLengthFromStringBufferPipe][Error] Comprobacion del tipo de la variable data");
                }
                return(ifelse(nchar_conf,nchar(data),object.size(data)))
            },
            
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),  
        private = list(
            propertyName = "length"
        )
    )
}