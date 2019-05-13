{
rm(list = ls()) 
#Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")
source("content-preprocessorinr/config/sourceLoad.R")

# bdp4R_object <- Bdp4R$new()
# bdp4R_object$proccess_files(pathFiles = "content-preprocessorinr/testFiles/tests",
#                             pipe = SerialPipes$new())

bdp4R_execute(pathFiles = "content-preprocessorinr/testFiles/test",
              pipe = SerialPipes$new()
             )

}