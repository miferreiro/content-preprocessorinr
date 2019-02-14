{
rm(list = ls()) 

inicio <- Sys.time()
#Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")

source("content-preprocessorinr/config/sourceLoad.R")

out <- "basuraPruebas/aux.csv"
listInstances <- proccess_files("content-preprocessorinr/testFiles/tests/hsspam14", SerialPipes$new(), pathOutPut = out)

fin <- Sys.time()
cat("[main][Info] ", "Start processing: ", paste(inicio) ,"\n")
cat("[main][Info] ", "End processing: ", paste(fin), "\n")
}
