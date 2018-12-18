ExtractorEml <- R6Class(
    classname = "ExtractorEml",
    inherit = ExtractorSource,
    public = list(
        initialize = function(path) {
            super$initialize(path)
            super$addProperties(generalFun$getTarget(super$getPath()),"target")
            super$obtainSourceDate()
            ifelse(!(validUTF8(super$getSource())),
                   {  
                       mensaje <- c( "el archivo " , super$getPath() , " no es utf8")
                       warning(mensaje)
                   }
           ,"")
        },
        obtainDate = function(){
            date <- tryCatch(read_emails(self$getPath())@date,
                                     warning = function(w) {
                                         print("Date eml warning");
                                         print("");
                                     },
                                     error = function(e) {
                                         print(c("Date eml error",self$getPath()));
                                         print("");
                                     })
            
                formato1 = "%a, %d %b %Y %H:%M:%S %z";
                date <- as.POSIXct(date,format = formato1)
                formato2 <- "%a %b %d %H:%M:%S %Z %Y"
                private$date <- format(date,formato2)
        
            
        },       
        obtainSource = function(){
            private$source <- tryCatch(enc2utf8(read_emails(self$getPath())@message),
                                       warning = function(w) {
                                           print("Source eml warning");
                                           print("");
                                       },
                                       error = function(e) {
                                           print(c("Date eml error",self$getPath()));
                                           print("");
                                       })
        }
    )
)
