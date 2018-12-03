DataSms <- R6Class(
    classname = "DataSms",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        getDate = function(){
            return(private$date)
        },
        getPath = function(){
            return(private$path);
        },
        getData = function(){
            return(private$data)
        },
        obtainSource = function(){
            private$source <-  readLines(self$getPath())
        },
        obtainDate = function(...){
           private$date = NULL
        },
        getSource = function(){
            return(private$source)
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