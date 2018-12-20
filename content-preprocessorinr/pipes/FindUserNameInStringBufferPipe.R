{
    FindUserNameInStringBufferPipe <- R6Class(
        "FindUserNameInStringBufferPipe",
        
        public = list(
            userPattern = "((?:\\s|^|[\"¿¡])(@[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:\\?\"!,.]?(?=(?:\\s|$)))",
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[FindUserNameInStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getData() %>>% 
                    self$findUserName() %>>%
                        {instancia$setData(.)}
                
                return(instancia);
            },
            
            findUserName = function(data){
                return(str_replace_all(data,
                                       regex(self$userPattern,
                                             ignore_case = TRUE,
                                             multiline = TRUE), ""))
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