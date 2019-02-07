{
rm(list = ls()) 

 
inicio <- Sys.time()
Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")

source("content-preprocessorinr/config/sourceLoad.R")

out <- "RDatas/all.RData"
dataFrame <- proccess_files("content-preprocessorinr/testFiles/tests", SerialPipes$new(),pathOutPut = out)
pruebaAll <- readRDS(file = out)
fin <- Sys.time()
cat("Inicio procesamiento: " ,paste(inicio) ,"\n")
cat("Fin procesamiento: " ,paste(fin) ,"\n")
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
# keys <- read.ini("content-preprocessorinr/config/configurations.ini")
# 
# library("rtweet")
# 
# p <- create_token(
#   app = "my_twitter_research_app",
#   consumer_key = keys$twitter$ConsumerKey,
#   consumer_secret = keys$twitter$ConsumerSecret,
#   access_token = keys$twitter$AcessToken,
#   access_secret = keys$twitter$AccessTokenSecret) #Funcion para la conexion, fijate que no es igual que la otra. He quiitado las claves por eso de la privacidad (aunque dropbox es lo mas inseguro que hay pero bueno).
# 
# 
# a <- rtweet::lookup_tweets("1016748849053011970")
# a$lang
# for(i in 1:400) {print(ValidInstancesList[[i]][[".__enclos_env__"]][["private"]][["properties"]][["Emojis"]])}
# for(i in 1:300) {print(ValidInstancesList[[i]][[".__enclos_env__"]][["private"]][["data"]])}
