ExtractorSms <- R6Class(
    classname = "ExtractorSms",
    inherit = ExtractorSource,
    public = list(
        initialize = function(path) {
            super$initialize(path)
           #  super$addProperties(generalFun$getTarget(super$getPath()),"target")
           #  
           #  super$obtainSourceDate()
           #  ifelse(!(validUTF8(super$getSource())),
           #         {  
           #             mensaje <- c( "el archivo " , super$getPath() , " no es utf8")
           #             warning(mensaje)
           #         }
           # ,"")
        },
        obtainSource = function(){
            private$source <-  enc2utf8(readLines(self$getPath()))
        },
        obtainDate = function(){
           private$date = ""
        }
    )
)