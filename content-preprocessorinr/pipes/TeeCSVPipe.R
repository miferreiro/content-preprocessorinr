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
      #contains the name of the property that will be obtained in the pipe
      #In addition, the name of the property of the language is indicated, 
      #and the place where the resources of the interjections are stored. 
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
    
    pipe = function(instance, withData = TRUE, withSource = TRUE) {
      #
      #Function that complete the data.frame with the preprocessed instance
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   withData: (logical) indicate if the data is added to data.frame
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
      
      instance$addFlowPipes("TeeCSVPipe")
      
      if (!instance$checkCompatibility("TeeCSVPipe", self$getAlwaysBeforeDeps())) {
        stop("[TeeCSVPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      if (!instance$isInstanceValid()) {
        return(instance)
      }
      
      row <- c()
      rowNames <- c()
      
      path <- instance$getPath()
      row <- c(row, path)
      rowNames <- c(rowNames, "path")
      
      if (withData) {
        data <- instance$getData()
        row <- c(row, data)
        rowNames <- c(rowNames, "data")
      }
      
      if (withSource) {
        source <- as.character(paste0(unlist(instance$getSource())))
        row <- c(row, source)
        rowNames <- c(rowNames, "source")
      }
      
      date <- instance$getDate()
      row <- c(row, date)
      rowNames <- c(rowNames, "date")
      
      for (name in instance$getNamesOfProperties()) { 
        rowNames <- c(rowNames, name)
      }
      
      for (property in instance$getProperties()) { 
        row <- c(row, paste0(unlist(property), collapse = "|"))
      }
      
      names(row) <- rowNames

      dataFrameAll <<- rbind(dataFrameAll, rbind(row), make.row.names = F, stringsAsFactors = FALSE)
      
      return(instance)
    }
  )
)

