{
    FindHashtagInStringBufferPipe <- R6Class(
        
        "FindHashtagInStringBufferPipe",
        
        public = list(
            
            hashtagPattern = "(?:\\s|^|[\"¿¡])(#[^\\p{Cntrl}\\p{Space}!\"#$%&'()*+\\\\,\\/:;<=>?@\\[\\]^`{|}~.-]+)[;:?\"!,.]?(?=(?:\\s|$))",
            
            pipe = function(instance,removeHashtag = TRUE){
                
                if (!"ExtractorSource" %in% class(instance)) {
                    stop("[FindHashtagInStringBufferPipe][pipe][Error]
                         Checking the type of the variable: instance ", class(instance));
                }
            
                if (!"logical" %in% class(removeHashtag)) {
                    stop("[FindHashtagInStringBufferPipe][pipe][Error]
                         Checking the type of the variable: removeHashtag ", class(removeHashtag));
                }
                
                instance$getData() %>>% 
                    self$findHashtag() %>>%
                        unlist() %>>%
                            {instance$addProperties(.,self$getPropertyName())}
                
                if (removeHashtag) {
                    instance$getData()  %>>%
                        self$replaceHashtag() %>>%
                            {instance$setData(.)}
                }    
                
                return(instance);
            },
            
            replaceHashtag = function(data){
                
                if (!"character" %in% class(data)) {
                    stop("[FindHashtagInStringBufferPipe][replaceHashtag][Error] 
                         Checking the type of the variable: data ", class(data));
                }
                
                return(str_replace_all(data,
                                       regex(self$hashtagPattern,
                                             ignore_case = TRUE,
                                             multiline = TRUE), " "))
            },
            
            findHashtag = function(data){
                
                if (!"character" %in% class(data)) {
                    stop("[FindHashtagInStringBufferPipe][findHashtag][Error] 
                         Checking the type of the variable: data ", class(data));
                }
                
                return(str_extract_all(data,
                                    regex(self$hashtagPattern,
                                          ignore_case = TRUE,
                                          multiline = TRUE)))
            },
            
            getPropertyName = function(){
                return(private$propertyName)
            }
        ),  
        private = list(
            propertyName = "hashtag"
        )
    )
}