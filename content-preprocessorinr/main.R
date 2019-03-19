{
rm(list = ls()) 
#Sys.setlocale("LC_TIME","UK")#Sys.setlocale("LC_TIME","Spanish")
source("content-preprocessorinr/config/sourceLoad.R")

out <- "outputSms-Spam-Collection.csv"
outSynsets <- "outputSms-Spam-Collection-Synsets.csv"
# listInstances <- proccess_files("content-preprocessorinr/testFiles/tests/sms-spam-collection",
#                                 SerialPipes$new(), pathOutPut = out,
#                                 pathOutPutSynsets = outSynsets)
# bdp4R_object <- Bdp4R$new()
# bdp4R_object$proccess_files("content-preprocessorinr/testFiles/tests/basic",
#                              SerialPipes$new(),
#                              pathOutPut = out,
#                              pathOutPutSynsets = outSynsets)

bdp4R_execute("content-preprocessorinr/testFiles/tests/basic",
                                           SerialPipes$new(),
                                           pathOutPut = out,
                                           pathOutPutSynsets = outSynsets)
}
