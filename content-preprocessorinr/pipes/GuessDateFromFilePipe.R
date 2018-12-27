{
    GuessDateFromFilePipe <- R6Class(
        
        "GuessDateFromFilePipe",
        
        public = list(
            
            pipe = function(instance){
                
                if (!"ExtractorSource" %in% class(instance)) {
                    stop("[GuessDateFromFilePipe][pipe][Error] 
                         Checking the type of the variable: instance ", class(instance));
                }
                
                instance$obtainDate();
                
                return(instance);
            },
            
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),
        private = list(
            propertyName = "date"
        )
    )
}