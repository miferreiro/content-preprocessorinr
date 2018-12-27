{
    FindUserNameInStringBufferPipe <- R6Class(
        
        "FindUserNameInStringBufferPipe",
        
        public = list(
            
            userPattern = "((?:\\s|^|[\"¿¡])(@[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~]+)[;:\\?\"!,.]?(?=(?:\\s|$)))",
            
            pipe = function(instance,removeUser = TRUE){

                if (!"ExtractorSource" %in% class(instance)) {
                    stop("[FindUserNameInStringBufferPipe][pipe][Error]
                         Checking the type of the variable: instance ", class(instance));
                }
                
                if (!"logical" %in% class(removeUser)) {
                    stop("[FindUserNameInStringBufferPipe][pipe][Error]
                         Checking the type of the variable: removeUser ", class(removeUser));
                }
                               
                instance$getData() %>>% 
                    self$findUserName() %>>%
                        unlist() %>>%
                            {instance$addProperties(.,self$getPropertyName())};
                
                if (removeUser) {
                    instance$getData()  %>>%
                        self$replaceUserName() %>>%
                            instance$setData();
                }
                
                return(instance);
            },
            
            replaceUserName = function(data){
                
                if (!"character" %in% class(data)) {
                    stop("[FindUserNameInStringBufferPipe][replaceUserName][Error] 
                         Checking the type of the variable: data ", class(data));
                }
                
                return(str_replace_all(data,
                                       regex(self$userPattern,
                                             ignore_case = TRUE,
                                             multiline = TRUE), " "))
            },
            
            findUserName = function(data){
                
                if (!"character" %in% class(data)) {
                    stop("[FindUserNameInStringBufferPipe][findUserName][Error] 
                         Checking the type of the variable: data ", class(data));
                }
                
                return(str_extract_all(data,
                                       regex(self$userPattern,
                                             ignore_case = TRUE,
                                             multiline = TRUE)))
            },
            
            getPropertyName = function(){
                return(private$propertyName)
            }
            
        ),  
        private = list(
            propertyName = "@userName"
        )
        
        
    )
}
