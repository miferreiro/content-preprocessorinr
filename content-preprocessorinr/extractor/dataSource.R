{
DataSource <- R6Class(
    "DataSource",
    public = list(
        obtainSource = function(){stop("I'm an abstract interface method")},
        obtainDate = function(){stop("I'm an abstract interface method")},
        getDate = function(){stop("I'm an abstract interface method")},
        getSource = function(){stop("I'm an abstract interface method")},
        getPath = function(){stop("I'm an abstract interface method")},
        getProperties = function(){stop("I'm an abstract interface method")},
        createInstance = function(path){
            switch(file_ext(path),
                   `eml` =  return(DataEml$new(path)),
                   `tsms` = return(DataSms$new(path)),
                   `twtid` = return(DataTwtid$new(path)),
                   `warc` = return(DataWarc$new(path)),
                   `tytb` = return(DataTytb$new(path)),
                   `ytbid` = return(DataYtbid$new(path))
            )
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
}