Builder <- R6Class(
    
    "Builder",
    
    public = list(
        
        initialize = function() {
            
        },
        
        createInstance = function(path){
            
            if (!"character" %in% class(path)) {
                stop("[Builder][createInstance][Error] 
                     Checking the type of the variable: path ", class(path));
            }
            
            switch(file_ext(path),
                   `eml` =  return(ExtractorEml$new(path)),
                   `tsms` = return(ExtractorSms$new(path)),
                   `twtid` = return(ExtractorTwtid$new(path)),
                   `ttwt` = return(ExtractorTtwt$new(path)),
                   `warc` = return(ExtractorWarc$new(path)),
                   `tytb` = return(ExtractorTytb$new(path)),
                   `ytbid` = return(ExtractorYtbid$new(path))
            )
        }
    )
)