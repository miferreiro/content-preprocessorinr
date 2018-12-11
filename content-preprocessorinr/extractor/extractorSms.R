ExtractorSms <- R6Class(
    classname = "ExtractorSms",
    inherit = ExtractorSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainSource = function(){
            private$source <-  enc2utf8(readLines(self$getPath()))
        },
        obtainDate = function(){
           private$date = ""
        }
    )
)