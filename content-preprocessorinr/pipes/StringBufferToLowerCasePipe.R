#Class to convert the data to lowercase
#
#Variables:
#
StringBufferToLowerCasePipe <- R6Class(
    
  "StringBufferToLowerCasePipe",
  
  inherit = PipeGeneric,
    
  public = list(
    
    initialize = function(propertyName = "",  
                          alwaysBeforeDeps = list(), 
                          notAfterDeps = list("AbbreviationFromStringBufferPipe", 
                                              "SlangFromStringBufferPipe")) {
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
        stop("[StringBufferToLowerCasePipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      
      if (!"list" %in% class(alwaysBeforeDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
                Checking the type of the variable: alwaysBeforeDeps ", 
                  class(alwaysBeforeDeps))
      }
      
      if (!"list" %in% class(notAfterDeps)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
                Checking the type of the variable: notAfterDeps ", 
                  class(notAfterDeps))
      }
      
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
    },        
    
    pipe = function(instance) {
      #
      #Function that preprocesses the instance to convert the data to lowercase
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #               
      if (!"Instance" %in% class(instance)) {
          stop("[StringBufferToLowerCasePipe][pipe][Error] 
                  Checking the type of the variable: instance ", 
                    class(instance))
      }
      
      instance$addFlowPipes("StringBufferToLowerCasePipe")
      
      if (!instance$checkCompatibility("StringBufferToLowerCasePipe", self$getAlwaysBeforeDeps())) {
        stop("[StringBufferToLowerCasePipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$addBanPipes(unlist(super$getNotAfterDeps()))
      
      instance$getData() %>>% 
        self$toLowerCase() %>>%
          instance$setData()
      
      return(instance)
    },
        
    toLowerCase = function(data) {
      #
      #Function that convert the data to lowercase
      #
      #Args:
      #   data: (character) text to preproccess
      #Returns:
      #   Data in lowercase
      #     
      if (!"character" %in% class(data)) {
          stop("[StringBufferToLowerCasePipe][toLowerCase][Error] 
                  Checking the type of the variable: data ",
                    class(data))
      }
      
      return(data %>>% tolower())
    }
  )
)
