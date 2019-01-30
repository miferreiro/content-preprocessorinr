{
rm(list = ls()) 
t <- proc.time()

Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")
#Carga todos los archivos .R
source("content-preprocessorinr/config/sourceLoad.R")
#Inicializamos el objeto que manejará los diferentes tipos de cosnexiones: youtube y twitter
connections <- Connections$new()
Files <- list.files(path = "content-preprocessorinr/testFiles/tests",
                    recursive = TRUE,
                    full.names = TRUE,
                    all.files = TRUE)
#Creamos la lista de instancias, las cuales contendran el date, source, path,data y una lista de propiedades
#del archivo que se encuentra en el path indicado
InstancesList <- sapply(Files, FactoryMethod$new()$createInstance)
cat("Se han creado: ",length(InstancesList)," instancias.\n")
InstancesList <- sapply(InstancesList, SerialPipes$new()$pipeAll)


ValidInstancesList <- list()
InvalidInstancesList <- list()
invalidBooleanList <- list()

# Obtenemos la lista de instanciasInvalidas
invalidBooleanList <- lapply(InstancesList, deleteInvalidInstances)
# A partir de la lista instancias invalidas y las lista de instancias originales, obtenemos la lista de instancias validas
ValidInstancesList <- obtainValidInstances(InstancesList, invalidBooleanList)
InvalidInstancesList <- obtainInvalidInstances(InstancesList, invalidBooleanList)
# View(ValidInstancesList)
# print(ValidInstancesList[["content-preprocessorinr/testFiles/tests/smsspamcollection/_spam_/ejemplo.tsms"]][[".__enclos_env__"]][["private"]][["properties"]][["abbreviation"]])
# print(ValidInstancesList[["content-preprocessorinr/testFiles/tests/smsspamcollection/_spam_/ejemplo.tsms"]][[".__enclos_env__"]][["private"]][["source"]])
# print(ValidInstancesList[["content-preprocessorinr/testFiles/tests/smsspamcollection/_spam_/ejemplo.tsms"]][[".__enclos_env__"]][["private"]][["data"]])
# cat("Time: ",proc.time() - t )

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

# warcs <- list()
# 
# for (ins in ValidInstancesList) {
# 
#   warcs <- list.append(warcs, ins[[".__enclos_env__"]][["private"]][["source"]])
# }
# 
# names(warcs) <- names(ValidInstancesList)
# save(warcs,file = "warcs.RData")
# rm(warcs)
# load("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorinr/warcs.RData")

# emls <- list()
# 
# for (ins in ValidInstancesList) {
# 
#   emls <- list.append(emls , list(ins[[".__enclos_env__"]][["private"]][["source"]], ins[[".__enclos_env__"]][["private"]][["data"]]))
# }
# 
# names(emls) <- names(ValidInstancesList)
# save(emls,file = "emls.RData")
# # rm(emls)
# load("C:/Users/Miguel/Desktop/cosas de R/content-preprocessorinr/warcs.RData")

# keys <- read.ini("content-preprocessorinr/config/configurations.ini")
# install.packages("rtweet")
# library("rtweet")
# 
# create_token(
#   app = "my_twitter_research_app",
#   consumer_key = keys$twitter$ConsumerKey,
#   consumer_secret = keys$twitter$ConsumerSecret,
#   access_token = keys$twitter$AcessToken,
#   access_secret = keys$twitter$AccessTokenSecret) #Funcion para la conexion, fijate que no es igual que la otra. He quiitado las claves por eso de la privacidad (aunque dropbox es lo mas inseguro que hay pero bueno).
# 
# 
# a <- rtweet::lookup_tweets("1016748849053011970")
# a$lang


