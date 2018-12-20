{
    FindUrlInStringBufferPipe <- R6Class(
        "FindUrlInStringBufferPipe",
        public = list(
            URLPattern = '((?:\\s|^)(?:(?:[a-z0-9]+:)(?:\\/\\/|\\/|)?|(www.))(?:[\\w-]+(?:(?:\\.[\\w-]+)+))(?:[\\w.,@?^=%&:\\/~+#-]*[\\w@?^=%&\\/~+#-])?(?=(?:,|;|!|:|\"|\\?|\\s|$)))',
            
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[FindUrlInStringBufferPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getData() %>>% 
                    self$findUrl() %>>%
                        {instancia$setData(.)}
                
                return(instancia);
            },
            
            findUrl = function(data){
                return(str_replace_all(data,
                                       regex(self$URLPattern,
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