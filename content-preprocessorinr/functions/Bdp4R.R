Bdp4R <- R6Class(
  
  "Bdp4R",
  
  public = list(
    
    initialize = function(pathKeys = "content-preprocessorinr/config/configurations.ini") {
      
      if (!"character" %in% class(pathKeys)) {
        stop("[Bdp4R][initialize][Error] 
                Checking the type of the variable: pathKeys ", 
                  class(pathKeys))
      }  
      
      if (!"ini" %in% file_ext(pathKeys)) {
        stop("[Bdp4R][initialize][Error]
                Checking the extension of the file: pathKeys ",
                  file_ext(pathKeys))
      }
      
      Bdp4R[["private_fields"]][["resourceHandle"]] <- ResourceHandler$new()
      Bdp4R[["private_fields"]][["dataFrameAll"]] <- data.frame()
      Bdp4R[["private_fields"]][["dataFrameAllSynsets"]] <- data.frame()
      Bdp4R[["private_fields"]][["connections"]] <- Connections$new(pathKeys)
      Bdp4R[["private_fields"]][["babelUtils"]] <- BabelUtils$new(pathKeys)
      
      Bdp4R[["private_fields"]][["synsetDictionary"]] <- SynsetDictionary$new()
      
    },

    proccess_files = function(pathFiles, 
                              pipe, 
                              pathOutPut = "output.RData",
                              pathOutPutSynsets = "outputSynsets.RData") {
      
      if (!"character" %in% class(pathFiles)) {
        stop("[Bdp4R][proccess_files][Error] 
                Checking the type of the variable: pathFiles ", 
                  class(pathFiles))
      } 
      
      if (!"TypePipe" %in% class(pipe)) {
        stop("[Bdp4R][proccess_files][Error] 
                Checking the type of the variable: pipe ", 
                  class(pipe))
      } 
      
      if (!"character" %in% class(pathOutPut)) {
        stop("[Bdp4R][proccess_files][Error] 
                Checking the type of the variable: pathOutPut ", 
                  class(pathOutPut))
      }  
      
      if (!file_ext(pathOutPut) %in% "csv" & !file_ext(pathOutPut) %in% "RData") {
        stop("[Bdp4R][proccess_files][Error] 
                Checking the extension of the variable: pathOutPut",
                  file_ext(pathKeys))
      }
      
      if (!"character" %in% class(pathOutPutSynsets)) {
        stop("[Bdp4R][proccess_files][Error] 
                Checking the type of the variable: pathOutPutSynsets ", 
                  class(pathOutPutSynsets))
      }  
      
      if (!file_ext(pathOutPutSynsets) %in% "csv" & !file_ext(pathOutPutSynsets) %in% "RData") {
        stop("[Bdp4R][proccess_files][Error] 
                Checking the extension of the variable: pathOutPutSynsets",
                  file_ext(pathOutPutSynsets))
      }
      #Array of files to preprocess
      Files <- list.files(path = pathFiles, recursive = TRUE, full.names = TRUE, all.files = TRUE)
      #Create the list of instances, which will contain the date, source, path, data 
      #and a list of properties of the file that is in the indicated path
      InstancesList <- sapply(Files, InstanceFactory$new()$createInstance)
      cat("[Bdp4R][proccess_files][Info] ", "Has been created: ", length(InstancesList)," instances.\n")
      listInstances <- sapply(InstancesList, pipe$pipeAll)
      
      if (file_ext(pathOutPut) %in% "csv") {
        write.table(x = Bdp4R[["private_fields"]][["dataFrameAll"]], 
                    file = pathOutPut, 
                    sep = ";", quote = T,
                    col.names = TRUE, row.names = FALSE,
                    qmethod = c("double"),
                    fileEncoding = "UTF-8")
      } else {
        
        if (file_ext(pathOutPut) %in% "RData") {
          saveRDS(Bdp4R[["private_fields"]][["dataFrameAll"]], 
                  file = pathOutPut)
        } else {
          stop("[Bdp4R][proccess_files][Error] 
                  Checking the extension of the variable: pathOutPut",
                    file_ext(pathOutPut))
        }
      }
      
      if (file_ext(pathOutPutSynsets) %in% "csv") {
        write.table(x = Bdp4R[["private_fields"]][["dataFrameAllSynsets"]], 
                    file = pathOutPutSynsets, 
                    sep = ";", quote = T,
                    col.names = TRUE, row.names = FALSE,
                    qmethod = c("double"),
                    fileEncoding = "UTF-8")
      } else {
        
        if (file_ext(pathOutPutSynsets) %in% "RData") {
          saveRDS(Bdp4R[["private_fields"]][["dataFrameAllSynsets"]], 
                  file = pathOutPutSynsets)
        } else {
          stop("[Bdp4R][proccess_files][Error] 
                  Checking the extension of the variable: pathOutPutSynsets",
                    file_ext(pathOutPutSynsets))
        }
      }
      
      return(listInstances)
    }
  ),
  
  private = list(
    #Initialize the object that handles the different types of connections with youtube and twitter
    connections = NULL,
    #Initialize the object that handles the different types of connections with babelfy and babelnet
    babelUtils = NULL,
    #Initialize the object that manages the loading of the resource files, such as 
    #abbreviation, slang, stopword, etc.
    resourceHandle = NULL,
    #Variable in which the data.frame generated by the pipes will be saved
    dataFrameAll = NULL,
    #Variable in which a data.frame is built with the instance synsets
    dataFrameAllSynsets = NULL,
    
    synsetDictionary = NULL
  )
)