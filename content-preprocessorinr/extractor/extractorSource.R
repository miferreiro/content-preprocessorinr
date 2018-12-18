ExtractorSource <- R6Class(
    "ExtractorSource",
    public = list(
        initialize = function(path) {
            if (!"character" %in% class(path)){
                stop("[ExtractorSource][Error] Comprobacion del tipo de la variable path");
            }
            private$path <- path
        },
        obtainSource = function(){stop("I'm an abstract interface method")},
        obtainDate = function(){stop("I'm an abstract interface method")},
        obtainSourceDate = function(){
            tryCatch({
            self$obtainSource();
            self$obtainDate();
        },
        warning = function(w){
            cat("Warning Source ",private$path,"\n");
            print(w)},
        error = function(e){
            cat("Error Source  ",private$path,"\n");
            print(e)})
        },
        getDate = function(){
            return(private$date);
        },
        getSource = function(){
            return(private$source);
        },
        getPath = function(){
            return(private$path);
        },
        getData = function(){
            return(private$data);
        },
        getProperties = function(){
            return(private$properties)
        },
        addProperties = function(valorPropiedad,nombrePropiedad){
            if (!"character" %in% class(nombrePropiedad)){
                stop("[ExtractorSource][Error] Comprobacion del tipo de la variable nombrePropiedad");
            }
            private$properties <-  list.append(private$properties,valorPropiedad)
            names(private$properties)[length(self$getProperties())] <- nombrePropiedad
        },
        getSpecificProperties = function(nombrePropiedad){
            if (!"character" %in% class(nombrePropiedad)){
                stop("[ExtractorSource][Error] Comprobacion del tipo de la variable nombrePropiedad");
            }
            return(private$properties[[nombrePropiedad]])
        },
        setSpecificProperties = function(nombrePropiedad,valorPropiedad){
            if (!"character" %in% class(nombrePropiedad)){
                stop("[ExtractorSource][Error] Comprobacion del tipo de la variable nombrePropiedad");
            }
            private$properties[[nombrePropiedad]] <- valorPropiedad        
        },
        setData = function(data){
            private$data = data
        }
    ),
    private = list(
        date = "",
        source = "",
        path = "",
        data = "",
        properties = list()
    )
)