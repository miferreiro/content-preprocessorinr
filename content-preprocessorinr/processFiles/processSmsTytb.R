{ 
    SmsTytbFiles <- list.files(path = filesTestPath
                               ,pattern = "tsms|tytb"
                               ,recursive = TRUE
                               ,full.names = TRUE
                               ,all.files = TRUE)
    
    SmsTytbInstancesList <- list();
    
    SmsTytbInstancesList <- sapply(SmsTytbFiles, ExtractorSource$public_methods$createInstance)
    
    invisible(sapply(SmsTytbInstancesList,propertiesSourceDate))
    rm(SmsTytbFiles)
    return(SmsTytbInstancesList);
}