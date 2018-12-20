ExtractorTytb <- R6Class(
    classname = "ExtractorTytb",
    inherit = ExtractorSource,
    public = list(
        initialize = function(path) {
            super$initialize(path)
            # super$addProperties(generalFun$getTarget(super$getPath()),"target")
            # super$obtainSourceDate()
            # ifelse(!(validUTF8(super$getSource())),
            #        {  
            #            mensaje <- c( "el archivo " , super$getPath() , " no es utf8")
            #            warning(mensaje)
            #        }
            #        ,"")
        },
        obtainDate = function(){
            private$date <- ""
        },
        obtainSource = function(){
            private$source <- enc2utf8(readLines(self$getPath()))
        }
    )
    
)