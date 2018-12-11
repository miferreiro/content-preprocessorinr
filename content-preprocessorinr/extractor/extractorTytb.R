ExtractorTytb <- R6Class(
    classname = "ExtractorTytb",
    inherit = ExtractorSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainDate = function(){
            private$date <- ""
        },
        obtainSource = function(){
            private$source <- enc2utf8(readLines(self$getPath()))
        }
    )
    
)