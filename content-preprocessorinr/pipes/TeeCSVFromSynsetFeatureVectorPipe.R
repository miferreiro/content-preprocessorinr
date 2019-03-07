#Class to complete the data.frame with the preprocessed instance and synsets
#
#Variables:
#
TeeCSVFromSynsetFeatureVectorPipe <- R6Class(
  
  "TeeCSVFromSynsetFeatureVectorPipe",
  
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
        stop("[TeeCSVFromSynsetFeatureVectorPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance, withData = TRUE, withSource = TRUE, 
                    listPropertySynsets = c("synsetVector", "synsetFeatureVector")) {
      #
      #Function that complete the data.frame with the preprocessed instance and synsets
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #   withData: (logical) indicate if the data is added to data.frame
      #   listPropertySynsets: (character) list indicating properties related to synsets
      # 
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #           
      if (!"Instance" %in% class(instance)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: instance ", 
                  class(instance))
      }
      
      if (!"logical" %in% class(withSource)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: withSource ", 
                  class(withSource))
      }
      
      if (!"logical" %in% class(withData)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: withData ", 
                  class(withData))
      }
      
      if (!"character" %in% class(listPropertySynsets)) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] 
                Checking the type of the variable: listPropertySynsets ", 
                  class(listPropertySynsets))
      }      
      
      instance$addFlowPipes("TeeCSVFromSynsetFeatureVectorPipe")
      
      if (!instance$checkCompatibility("TeeCSVFromSynsetFeatureVectorPipe", self$getAlwaysBeforeDeps())) {
        stop("[TeeCSVFromSynsetFeatureVectorPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      if (!instance$isInstanceValid()) {
        return(instance)
      }
      
      pos <- dim(dataFrameAllSynsets)[1] + 1
      
      
      dataFrameAllSynsets[pos, "path"] <<- instance$getPath()
      
      if (withData) {
        dataFrameAllSynsets[pos, "data"] <<- instance$getData()
      }
      
      if (withSource) {
        dataFrameAllSynsets[pos, "source"] <<- as.character(paste0(unlist(instance$getSource())))
      }
      
      dataFrameAllSynsets[pos, "date"] <<- instance$getDate()

      
      namesPropertiesList <- as.list(instance$getNamesOfProperties())
      names(namesPropertiesList) <- instance$getNamesOfProperties()
      
      for (name in list.remove(namesPropertiesList, listPropertySynsets)) { 
        dataFrameAllSynsets[pos, name] <<- paste0(unlist(instance$getSpecificProperty(name)), collapse = "|")
      }
      
      synsets <- instance$getSpecificProperty("synsetFeatureVector")
      
      synsetFeature <- synsets$getSynsetsFeature()
      
      for (synset in names(synsetFeature)) {
        dataFrameAllSynsets[pos, synset] <<- synsetFeature[[synset]]
      }
    
      return(instance)
    }
  )
)