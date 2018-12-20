{
    StoreFileExtensionPipe <- R6Class(
        
        "StoreFileExtensionPipe",
        
        public = list(
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[StoreFileExtensionPipe][Error] Comprobacion del tipo de la variable instancia");
                }
                
                instancia$getPath() %>>% 
                    self$getExtension() %>>%
                    {instancia$addProperties(.,self$getPropertyName())}
                
                return(instancia);
            },
            getExtension = function(path){
                if (!"character" %in% class(path)){
                    stop("[StoreFileExtensionPipe][Error] Comprobacion del tipo de la variable path");
                }
                return(file_ext(path))
            },
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),
        private = list(
            propertyName = "extension"
        )
    )
}