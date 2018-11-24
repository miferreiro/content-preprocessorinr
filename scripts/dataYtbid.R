DataYtbid<- R6Class(
    classname = "DataYtbid",
    inherit = DataSource,
    public = list(
        initialize = function(path) {
            private$path <- path
            self$obtainId();
        },
        id = "",
        obtainDate = function(){
            #private$date<- get_comments(filter = c(comment_id = id),textFormat = plainText)[publishedAt]
        },
        obtainId = function(){
            self$id <- readLines(self$getPath(),warn=FALSE)
        },
        obtainSource = function(){
           # private$source <- get_comments(filter = c(comment_id = id),textFormat = plainText)[value]
            private$source <- ""
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