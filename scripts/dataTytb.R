DataTytb <- R6Class(
    classname = "DataTytb",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
        },
        obtainDate = function(...){
            private$date <- as.character.Date(file.info(self$getPath())[["ctime"]],...)
        },
        obtainSource = function(){
            private$source <- readLines(self$getPath(),warn=FALSE)
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