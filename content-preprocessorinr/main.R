{
rm(list = ls()) 
t <- proc.time()
#setwd("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorInR")
Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")
#Carga todos los archivos .R
source("content-preprocessorinr/config/sourceLoad.R")
#Inicializamos el objeto que manejará los diferentes tipos de conexiones: youtube y twitter
connections <- Connections$new();
Files <- list.files(path = "content-preprocessorinr/testFiles/tests/www/_spam_"
                    ,recursive = TRUE
                    ,full.names = TRUE
                    ,all.files = TRUE)


#Creamos la lista de instancias, las cuales contendran el date, source, path,data y una lista de propiedades
#del archivo que se encuentra en el path indicado
InstancesList <- sapply(Files, FactoryMethod$new()$createInstance)

InstancesList <- sapply(InstancesList, SerialPipes$new()$pipeAll)


ValidInstancesList <- list();
invalidBooleanList <- list();

#Obtenemos la lista de instanciasInvalidas
invalidBooleanList <- lapply(InstancesList,deleteInvalidInstances)
#A partir de la lista instancias invalidas y las lista de instancias originales, obtenemos la lista de instancias validas
ValidInstancesList <- obtainValidInstances(InstancesList,invalidBooleanList)


cat("Time: ",proc.time() - t )

# for (aux in 1:length(ValidInstancesList)) { 
#   
#   if (ValidInstancesList[[aux]]
#   [[".__enclos_env__"]]
#   [["private"]]
#   [["properties"]]
#   [["length_after_cleaning_text"]] !=
#   
#   ValidInstancesList[[aux]]
#   [[".__enclos_env__"]]
#   [["private"]]
#   [["properties"]]
#   [["length_after_abbreviation"]]
#   
#   ) {
#     print("####")
#     print("path")
#     print("####")
#     print(ValidInstancesList[[aux]][[".__enclos_env__"]][["private"]][["path"]])
#     print("####")
#     print("source")
#     print("####")
#     print(ValidInstancesList[[aux]][[".__enclos_env__"]][["private"]][["source"]])
#     print("####")
#     print("data")
#     print("####")
#     print(ValidInstancesList[[aux]][[".__enclos_env__"]][["private"]][["data"]])
#     print("\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\")
#   }
#   
# }
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
