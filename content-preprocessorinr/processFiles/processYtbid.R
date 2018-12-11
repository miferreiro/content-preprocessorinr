{ 
    YtbidFiles <- list.files(path = filesTestPath
                             ,pattern = "ytbid"
                             ,recursive = TRUE
                             ,full.names = TRUE
                             ,all.files = TRUE)
    
    YtbidInstancesList <- list();

    YtbidInstancesList <- sapply(YtbidFiles, ExtractorSource$public_methods$createInstance)

    invisible(sapply(YtbidInstancesList,propertiesSourceDate))
    rm(YtbidFiles)
    return(YtbidInstancesList);
}