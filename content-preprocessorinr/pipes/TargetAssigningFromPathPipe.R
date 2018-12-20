{
    TargetAssigningFromPathPipe <- R6Class(
        "TargetAssigningFromPathPipe",
        public = list(
           pipe = function(instancia){
               if (!"ExtractorSource" %in% class(instancia)) {
                   stop("[TargetAssigningFromPathPipe][Error] Comprobacion del tipo de la variable instancia");
               }
               
               instancia$getPath() %>>% 
                   self$getTarget() %>>%
                        {instancia$addProperties(.,self$getPropertyName())}
               
               return(instancia);
           },
           getTarget = function(path,...) {
               if (!"character" %in% class(path)) {
                   stop("[TargetAssigningFromPathPipe][Error] Comprobacion del tipo de la variable path");
               }
               if (grepl("_ham_",path)) {
                   aux <- "ham"
               } else{
                   if (grepl("_spam_",path)) {
                       aux <- "spam"
                   } else{
                       aux <- "unrecognizable"
                   }
               }
               return(aux)
           },
           getPropertyName = function(){
               return(private$propertyName)
           }
        ),
           private = list(
               propertyName = "target"
           )
    )
}