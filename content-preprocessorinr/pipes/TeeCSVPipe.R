#Class to complete the data.frame with the preprocessed instance
#
#Variables:
#
TeeCSVPipe <- R6Class(
  
  "TeeCSVPipe",
  
  inherit = PipeGeneric,
  
  public = list(

    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #This constructor initialize the variable of propertyName.This variable 
      #contains the name of the property that will be obtained in the pipe.
      #
      #
      #Args:
      #   propertyName: (character) Name of the property
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #         
      if (!"character" %in% class(propertyName)) {
        stop("[TeeCSVPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TeeCSVPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TeeCSVPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance, withData = TRUE, withSource = TRUE, outPutPath = "dataFrameAll.csv") {
      #
      #Function that complete the data.frame with the preprocessed instance
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   withData: (logical) indicate if the data is added to data.frame
      #   outPutPath: (path) indicate the path to save properties of the instance
      # 
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(withSource)) {
        stop("[TeeCSVPipe][pipe][Error] 
                Checking the type of the variable: withSource ", 
                  class(withSource))
      }
      
      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVPipe][pipe][Error] 
                Checking the type of the variable: withData ", 
                  class(withData))
      }
      
      if (!"character" %in% class(outPutPath)) {
        stop("[TeeCSVPipe][pipe][Error] 
                Checking the type of the variable: outPutPath ", 
                  class(outPutPath))
      }
      
      if (!"csv" %in% file_ext(outPutPath)) {
        stop("[TeeCSVPipe][pipe][Error]
                Checking the extension of the file: outPutPath ",
                  file_ext(outPutPath))
      }
      
      instance$addFlowPipes("TeeCSVPipe")
      
      if (!instance$checkCompatibility("TeeCSVPipe", self$getAlwaysBeforeDeps())) {
        stop("[TeeCSVPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      if (!instance$isInstanceValid()) {
        return(instance)
      }
      
      if (file.exists(outPutPath)) {
        dataFrameAll <- read.csv(file = outPutPath, header = TRUE, 
                                     sep = ";", dec = ".", fill = FALSE, stringsAsFactors = FALSE)
      } else {
        dataFrameAll <- data.frame()
      }
      
      pos <- dim(dataFrameAll)[1] + 1
      
      dataFrameAll[pos, "path"] <- instance$getPath()
      
      if (withData) {
        dataFrameAll[pos, "data"] <- instance$getData()
      }
      
      if (withSource) {
        dataFrameAll[pos, "source"] <-
          as.character(paste0(unlist(instance$getSource())))
      }
      
      dataFrameAll[pos, "date"] <- instance$getDate()
      
      namesPropertiesList <- as.list(instance$getNamesOfProperties())
      names(namesPropertiesList) <- instance$getNamesOfProperties()
      
      for (name in namesPropertiesList) { 
        dataFrameAll[pos, name] <- 
          paste0(unlist(instance$getSpecificProperty(name)), collapse = "|")
      }
      
      write.table(x = dataFrameAll, 
                  file = outPutPath, 
                  sep = ";", 
                  dec = ".",
                  quote = T,
                  col.names = TRUE, 
                  row.names = FALSE,
                  qmethod = c("double"),
                  fileEncoding = "UTF-8")
      
      return(instance)
    }
  )
)