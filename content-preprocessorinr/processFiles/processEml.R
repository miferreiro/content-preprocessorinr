{ 
    EmlFiles <- list.files(path = filesTestPath
                           ,pattern = "eml"
                           ,recursive = TRUE
                           ,full.names = TRUE
                           ,all.files = TRUE)
    
    EmlInstancesList <- list();
    
    EmlInstancesList <- sapply(EmlFiles, ExtractorSource$public_methods$createInstance)
    
    invisible(sapply(EmlInstancesList,propertiesSourceDate))
    rm(EmlFiles)
    return(EmlInstancesList);
}