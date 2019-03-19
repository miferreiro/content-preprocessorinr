bdp4R_execute = function(pathKeys = "content-preprocessorinr/config/configurations.ini",
                         pathFiles, 
                         pipe = SerialPipes$new(), 
                         pathOutPut = "output.RData",
                         pathOutPutSynsets = "outputSynsets.RData") {

  if (!"character" %in% class(pathKeys)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pathKeys ",
              class(pathKeys))
  }
  
  if (!"ini" %in% file_ext(pathKeys)) {
    stop("[bdp4R_execute][Error]
            Checking the extension of the file: pathKeys ",
              file_ext(pathKeys))
  }
  
  if (!"character" %in% class(pathFiles)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pathFiles ",
              class(pathFiles))
  }
  
  if (!"TypePipe" %in% class(pipe)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pipe ",
              class(pipe))
  }
  
  if (!"character" %in% class(pathOutPut)) {
    stop("[proccess_files][Error]
            Checking the type of the variable: pathOutPut ",
              class(pathOutPut))
  }
  
  if (!file_ext(pathOutPut) %in% "csv" & !file_ext(pathOutPut) %in% "RData") {
    stop("[bdp4R_execute][Error]
            Checking the extension of the variable: pathOutPut",
              file_ext(pathKeys))
  }
  
  if (!"character" %in% class(pathOutPutSynsets)) {
    stop("[bdp4R_execute][Error]
            Checking the type of the variable: pathOutPutSynsets ",
              class(pathOutPutSynsets))
  }
  
  if (!file_ext(pathOutPutSynsets) %in% "csv" & !file_ext(pathOutPutSynsets) %in% "RData") {
    stop("[bdp4R_execute][Error]
            Checking the extension of the variable: pathOutPutSynsets",
              file_ext(pathOutPutSynsets))
  }  
  
  bdp4R_object <- Bdp4R$new(pathKeys)
  bdp4R_object$proccess_files(pathFiles,
                              pipe = pipe, 
                              pathOutPut = pathOutPut,
                              pathOutPutSynsets = pathOutPutSynsets)
}