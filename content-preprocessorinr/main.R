{
rm(list = ls()) 
#setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")
Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")
source("content-preprocessorinr/config/sourceLoad.R")
connections <- Connections$new();
Files <- list.files(path = "content-preprocessorinr/testFiles/tests"
                    ,recursive = TRUE
                    ,full.names = TRUE
                    ,all.files = TRUE)

InstancesList <- sapply(Files, Builder$new()$createInstance)
#ver paquete promises
pipes = function(x){
    x %>>% 
        TargetAssigningFromPathPipe$new()$pipe() %>>%
        StoreFileExtensionPipe$new()$pipe() %>>%
        GuessDateFromFilePipe$new()$pipe() %>>%
        File2StringBufferPipe$new()$pipe() %>>%
        MeasureLengthFromStringBufferPipe$new()$pipe() %>>%
        StripHTMLFromStringBufferPipe$new()$pipe() %>>%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_html_drop") %>>%
        FindUserNameInStringBufferPipe$new()$pipe() %>>%
        FindHashtagInStringBufferPipe$new()$pipe() %>>%
        FindUrlInStringBufferPipe$new()$pipe() %>>%
        FindEmoticonInStringBufferPipe$new()$pipe() %>>%
        FindEmojiInStringBufferPipe$new()$pipe() %>>%
        MeasureLengthFromStringBufferPipe$new()$pipe("length_after_cleaning_text") %>>%
        AbbreviationFromStringBufferPipe$new()$pipe() %>>%
        StringBufferToLowerCasePipe$new()$pipe() %>>%
        GuessLanguageFromStringBufferPipe$new()$pipe() %>>%
        InterjectionFromStringBufferPipe$new()$pipe() %>>%
        StopWordFromStringBufferPipe$new()$pipe() %>>%
        NERFromStringBufferPipe$new()$pipe() %>>%
        TeeCSVFromStringBufferPipe$new()$pipe() %>>% # new TeeCSVFromStringBufferPipe("output.csv", true),
        StringBuffer2SynsetVectorPipe$new()$pipe() %>>%
        # new SynsetVector2SynsetFeatureVectorPipe(SynsetVectorGroupingStrategy.COUNT),
        TeeCSVFromStringBufferPipe$new()$pipe() %>>% # new TeeCSVFromSynsetFeatureVectorPipe("outputsyns.csv"),
        {x}
}

InstancesList <- sapply(InstancesList, pipes)

ValidInstancesList <- list();
invalidBooleanList <- list();

invalidBooleanList <- lapply(InstancesList,deleteInvalidInstances)
ValidInstancesList <- obtainValidInstances(InstancesList,invalidBooleanList)

}

#
#parallelStart(mode = "multicore")
# parallelStart(mode = "local")
# parallelStartSocket(2)
# parallelSource(files = "content-preprocessorinr/config/sourceLoad.R")
# parallelExport(objnames = "generalFun");
# parallelExport(objnames = "connections");
# parallelExport(objnames = "builder");
# InstancesList <- sapply(Files, builder$createInstance)



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