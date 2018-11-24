{
    pipesFunctions <- R6Class(
        
        "FuncionesPipes",
        
        public = list(
            
            toLowerSource = function(texto){
                return(tolower(texto));
            }
        )
    )
}