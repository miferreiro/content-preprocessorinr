{
    GuessDateFromFilePipe <- R6Class(
        "GuessDateFromFilePipe",
        public = list(
            pipe = function(instancia){
                if (!"ExtractorSource" %in% class(instancia)) {
                    stop("[GuessDateFromFilePipe][Error] Comprobacion del tipo de la variable instancia");
                }
                instancia$obtainDate()
                
                return(instancia);
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