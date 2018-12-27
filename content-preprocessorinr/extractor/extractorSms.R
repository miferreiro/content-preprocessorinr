ExtractorSms <- R6Class(
    
    classname = "ExtractorSms",
    
    inherit = ExtractorSource,
    
    public = list(
        
        initialize = function(path) {
            super$initialize(path)
        },
        
        obtainSource = function(){
            private$source <-  enc2utf8(readLines(self$getPath()))
            self$setData(private$source)
        },
        
        obtainDate = function(){
           private$date = ""
        }
    )
)