
{rm(list = ls()) 
    
{    
#setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")
Sys.setlocale("LC_TIME","UK")
#Sys.setlocale("LC_TIME","Spanish")
source("content-preprocessorinr/config/pkgChecker.R")

source("content-preprocessorinr/config/connections.R")
source("content-preprocessorinr/extractor/extractorSource.R")
source("content-preprocessorinr/extractor/extractorSms.R")
source("content-preprocessorinr/extractor/extractorTwtid.R")
source("content-preprocessorinr/extractor/extractorWarc.R")
source("content-preprocessorinr/extractor/extractorEml.R")
source("content-preprocessorinr/extractor/extractorTytb.R")
source("content-preprocessorinr/extractor/extractorYtbid.R")
source("content-preprocessorinr/functions/pipesFunction.R")
source("content-preprocessorinr/functions/GeneralFunctions.R")
source("content-preprocessorinr/functions/Builder.R")
source("content-preprocessorinr/functions/invalidInstances.R")


# #EML
source("content-preprocessorinr/scripts/libraries/eml/eml.R")

# #WARC
source("content-preprocessorinr/scripts/libraries/warc-master/R/process_entry.r")
source("content-preprocessorinr/scripts/libraries/warc-master/R/process_info.r")
source("content-preprocessorinr/scripts/libraries/warc-master/R/process_request.r")
source("content-preprocessorinr/scripts/libraries/warc-master/R/process_response.r")
source("content-preprocessorinr/scripts/libraries/warc-master/R/read_warc_entry.r")


generalFun <- GeneralFunctions$new();
pipesFun <- pipesFunctions$new();
connections <- Connections$new();
builder <- Builder$new();
}
Files <- list.files(path = "content-preprocessorinr/testFiles/tests"
                        ,recursive = TRUE
                        ,full.names = TRUE
                        ,all.files = TRUE)

InstancesList <- sapply(Files, builder$createInstance)

ValidInstancesList <- list();
invalidBooleanList <- list();

invalidBooleanList <- lapply(InstancesList,deleteInvalidInstances)
ValidInstancesList <- obtainValidInstances(InstancesList,invalidBooleanList)

#invisible(sapply(ValidInstancesList,initialProperties))
}
# library(parallel)
# 
# numCores <- detectCores()
# cl <- makeCluster( numCores - 1)
# 
#  library(foreach)
#  sourceList <- list("content-preprocessorinr/processFiles/processTwtid.R",
#                     "content-preprocessorinr/processFiles/processYtbid.R",
#                     "content-preprocessorinr/processFiles/processWarc.R",
#                     "content-preprocessorinr/processFiles/processEml.R",
#                     "content-preprocessorinr/processFiles/processSmsTytb.R"
#                   )
# 
# 
# 
# 
#  invisible( foreach(x = sourceList
#                     ,.combine = 'list'
#                     ,.multicombine = TRUE
#                     ,.verbose =  FALSE
#                     ,.inorder = FALSE) %dopar%
#  {
#              filesTestPath = "content-preprocessorinr/testFiles/tests";
#              source("content-preprocessorinr/inicializacion.R");
#              source(x);
#              rm(x)
#              rm(filesTestPath)
# })
# 
# InstancesList <- unlist(list.append(EmlInstancesList
#                                     ,TwtidInstancesList
#                                     ,YtbidInstancesList
#                                     ,WarcInstancesList
#                                     ,SmsTytbInstancesList))
# 
# ValidInstancesList <- list();
# invalidBooleanList <- list();
# 
# invalidBooleanList <- lapply(InstancesList,deleteInvalidInstances)
# ValidInstancesList <- obtainValidInstances(InstancesList,invalidBooleanList)
# 
# invisible(sapply(ValidInstancesList,initialProperties))

#View(ValidInstancesList)

# {
# invisible(sapply(ValidInstancesList,pipes))
# 
# #Muestra las propiedades
# for (x in ValidInstancesList) {
#     print(x$getSource())
#     print("---------------------------------------------------")
#     print(x$getData())
#     print("|||||||||||||||||||||||||||||||||||||||||||||||||||")
# }
# 
# #Hacer csv
# #fun$toCsv(ValidInstancesList)
# }