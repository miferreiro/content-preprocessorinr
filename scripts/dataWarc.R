DataWarc <- R6Class(
    classname = "DataWarc",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainDate = function(...){
            private$date <-  tryCatch(read_warc_entry(self$getPath(),0)[[1]][['warc-date']],
                                      warning = function(w) {
                                          print("Date warc warning");
                                          print("");
                                      },
                                      error = function(e) {
                                          print(c("Date warc error",self$getPath()));
                                          print("");
                                      })
        },
        obtainSource = function(){
            private$source <-  tryCatch(read_warc_entry(self$getPath(),0)[[2]],
                                        warning = function(w) {
                                            print("Source warc warning");
                                            print("");
                                        },
                                        error = function(e) {
                                            print(c("Source warc error",self$getPath()));
                                            print("");
                                        })
            print(private$source)
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