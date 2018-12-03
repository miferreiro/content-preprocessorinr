DataEml <- R6Class(
    classname = "DataEml",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainDate = function(...){
            date <- tryCatch(read_emails(self$getPath())@date,
                                     warning = function(w) {
                                         print("Date eml warning");
                                         print("");
                                     },
                                     error = function(e) {
                                         print(c("Date eml error",self$getPath()));
                                         print("");
                                     })
            print(date)
            date <- as.POSIXct(date)
            print(date)
            formato <- "%a %b %d %H:%M:%S %Z %Y"
            private$date <- format(date,formato)
            print(private$date)
        },       
        obtainSource = function(){
            private$source <- tryCatch(read_emails(self$getPath())@message,
                                       warning = function(w) {
                                           print("Source eml warning");
                                           print("");
                                       },
                                       error = function(e) {
                                           print(c("Date eml error",self$getPath()));
                                           print("");
                                       })
        },
        getDate = function(){
            return(private$date)
        },
        getPath = function(){
            return(private$path);
        },
        getSource = function(){
            return(private$source)
        },
        getData = function(){
            return(private$data)
        },
        getProperties = function(){
            return(private$properties)
        },
        addProperties = function(valorPropiedad,nombrePropiedad){
            private$properties <-  list.append(private$properties,valorPropiedad)
            names(private$properties)[length(self$getProperties())] <- nombrePropiedad
        },
        getSpecificProperties = function(nombrePropiedad){
            return(private$properties[[nombrePropiedad]])
        },
        setSpecificProperties = function(nombrePropiedad,valorPropiedad){
            private$properties[[nombrePropiedad]] <- valorPropiedad        
        },
        setData = function(data){
            private$data = data
        }
    )
    )
