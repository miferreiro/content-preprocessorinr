DataTytb <- R6Class(
    classname = "DataTytb",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainDate = function(...){
            private$date <- ""
        },
        obtainSource = function(){
            private$source <- enc2utf8(readLines(self$getPath()))
        }
    )
    
)