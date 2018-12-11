{ 
rm(list = ls()) 

# library(parallel)
# 
# numCores <- detectCores()
# cl <- makeCluster( numCores - 1)

library(foreach)
sourceList <- list("content-preprocessorinr/processFiles/processTwtid.R",
                   "content-preprocessorinr/processFiles/processYtbid.R",
                   "content-preprocessorinr/processFiles/processWarc.R",
                   "content-preprocessorinr/processFiles/processEml.R",
                   "content-preprocessorinr/processFiles/processSmsTytb.R"
                 )

invisible( foreach(x = sourceList
                   ,.combine = 'list'
                   ,.multicombine = TRUE 
                   ,.verbose =  FALSE
                   ,.inorder = FALSE) %dopar% 
{
            filesTestPath = "content-preprocessorinr/testFiles/tests";
            source("content-preprocessorinr/inicializacion.R");
            source(x);
            rm(x)
            rm(filesTestPath)
})

InstancesList <- unlist(list.append(EmlInstancesList
                                    ,TwtidInstancesList
                                    ,YtbidInstancesList
                                    ,WarcInstancesList
                                    ,SmsTytbInstancesList))

ValidInstancesList <- list()
invalidBooleanList <- list();

invalidBooleanList <- lapply(InstancesList,deleteInvalidInstances)
ValidInstancesList <- obtainValidInstances(InstancesList,invalidBooleanList)

invisible(sapply(ValidInstancesList,initialProperties))

#View(ValidInstancesList)
}
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