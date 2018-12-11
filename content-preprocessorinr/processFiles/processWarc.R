{ 
    WarcFiles <- list.files(path = filesTestPath
                            ,pattern = "warc"
                            ,recursive = TRUE
                            ,full.names = TRUE
                            ,all.files = TRUE)
    
    WarcInstancesList <- list();
    
    WarcInstancesList <- sapply(WarcFiles, ExtractorSource$public_methods$createInstance)
    
    invisible(sapply(WarcInstancesList,propertiesSourceDate))
    rm(WarcFiles)
    return(WarcInstancesList);
}