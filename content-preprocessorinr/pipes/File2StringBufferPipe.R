{
    File2StringBufferPipe <- R6Class(
        
        "File2StringBufferPipe",
       
        public = list(
            
            pipe = function(instance){
                
                if (!"ExtractorSource" %in% class(instance)) {
                    stop("[File2StringBufferPipe][pipe][Error] 
                         Checking the type of the variable: instance ", class(instance));
                }
                
                instance$obtainSource();
                
                 ifelse(!(validUTF8(instance$getSource())),
                 {
                        message <- c( "The file: " , instance$getPath() , " isnt utf8")
                        warning(message)
                 }
                 ,"")
                
                return(instance);
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