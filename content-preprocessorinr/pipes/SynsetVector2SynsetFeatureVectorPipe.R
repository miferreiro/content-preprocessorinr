#Class to obtain the length of the data
#
# 
#
#Variables:
#
SynsetVector2SynsetFeatureVectorPipe <- R6Class(
  
  "SynsetVector2SynsetFeatureVectorPipe",
  
  inherit = PipeGeneric,
  
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list()) {
      #
      #Class constructor
      #
      #
      #Args:
      #   propertyName: (character) 
      #
      #Returns:
      #   null
      #            
      if (!"character" %in% class(propertyName)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][initialize][Error] 
             Checking the type of the variable: propertyName ", 
             class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][initialize][Error] 
             Checking the type of the variable: alwaysBeforeDeps ", 
             class(alwaysBeforeDeps))
      }
      if (!"list" %in% class(notAfterDeps)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][initialize][Error] 
             Checking the type of the variable: notAfterDeps ", 
             class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },
    
    pipe = function(instance,
                    propertyName = super$getPropertyName(),
                    nchar_conf = TRUE) {
      #
      #
      #
      #Args:
      #   instance: (Instance) instance to preprocces
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #          
      
      if (!"Instance" %in% class(instance)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][pipe][Error] 
             Checking the type of the variable: instance ", 
             class(instance))
      }
      
      if (!"character" %in% class(propertyName)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][pipe][Error] 
             Checking the type of the variable: propertyName ", 
             class(propertyName))
      }
      
      if (!"logical" %in% class(nchar_conf)) {
        stop("[SynsetVector2SynsetFeatureVectorPipe][pipe][Error] 
             Checking the type of the variable: nchar_conf ", 
             class(nchar_conf))
      }

      
      
      return(instance);
    }
  )
)
