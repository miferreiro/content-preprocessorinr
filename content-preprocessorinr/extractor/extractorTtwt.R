ExtractorTtwt <- R6Class(
    
    classname = "ExtractorTtwt",
    
    inherit = ExtractorSource,
    
    public = list(
        
        initialize = function(path) {
            super$initialize(path)
        },
        
        obtainDate = function(){
            private$date <- ""
        },
        
        obtainSource = function(){
            private$source <- enc2utf8(readLines(self$getPath()))
            self$setData(private$source)
        }
    )
)