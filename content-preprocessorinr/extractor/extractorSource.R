ExtractorSource <- R6Class(
    "ExtractorSource",
    public = list(
        obtainSource= function(){stop("I'm an abstract interface method")},
        obtainDate = function(){stop("I'm an abstract interface method")},
        obtainSourceDate = function(){
            self$obtainSource();
            self$obtainDate();
        },
        createInstance = function(path){
            switch(file_ext(path),
                   `eml` =  return(ExtractorEml$new(path)),
                   `tsms` = return(ExtractorSms$new(path)),
                   `twtid` = return(ExtractorTwtid$new(path)),
                   `warc` = return(ExtractorWarc$new(path)),
                   `tytb` = return(ExtractorTytb$new(path)),
                   `ytbid` = return(ExtractorYtbid$new(path))
            )
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
    ),
    private = list(
        date = "",
        source = "",
        path = "",
        data = "",
        properties = list()
    )
)