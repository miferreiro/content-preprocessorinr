{
    FindHashtagInStringBufferPipe <- R6Class(
        "FindHashtagInStringBufferPipe",
        
        public = list(
            hashtagPattern = "(?:\\s|^|[\"¿¡])(#[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.]?(?=(?:\\s|$))",
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[FindHashtagInStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getData() %>>% 
                    self$findHashtag() %>>%
                        {instancia$setData(.)}
                
                return(instancia);
            },
            
            findHashtag = function(data){
                return(str_replace_all(data,
                                       regex(self$hashtagPattern,
                                             ignore_case = TRUE,
                                             multiline = TRUE), " "))
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