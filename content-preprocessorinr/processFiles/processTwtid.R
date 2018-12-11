{
    TwtidFiles <- list.files(path = filesTestPath
                             ,pattern = "twtid"
                             ,recursive = TRUE
                             ,full.names = TRUE
                             ,all.files = TRUE)

    TwtidInstancesList <- list();

    TwtidInstancesList <- sapply(TwtidFiles, ExtractorSource$public_methods$createInstance)

    invisible(sapply(TwtidInstancesList,propertiesSourceDate))
    rm(TwtidFiles)
    return(TwtidInstancesList);
}