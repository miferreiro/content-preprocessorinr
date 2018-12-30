#Class to 
#
#
#Variables:
#propertyName: (character) 
#targets: (list) 
#
TargetAssigningFromPathPipe <- R6Class(
    
  "TargetAssigningFromPathPipe",
    
  public = list(
        
    initialize = function(targets = list("ham","spam"), targetName = list("_ham_","_spam_")) {
      
      private$targets <- targets
      names(private$targets) <- targetName
       
    },    
       
    pipe = function(instance) {
       
      if (!"ExtractorSource" %in% class(instance)) {
        stop("[TargetAssigningFromPathPipe][pipe][Error] 
             Checking the type of the variable: instance ", class(instance))
      }
     
      instance$getPath() %>>% 
        self$getTarget() %>>%
          {instance$addProperties(.,self$getPropertyName())}
     
      return(instance)
    },
     
    getTarget = function(path) {
       
      if (!"character" %in% class(path)) {
        stop("[TargetAssigningFromPathPipe][getTarget][Error] 
             Checking the type of the variable: path ", class(path))
      }
     
      for (target in names(self$getTargets())) {
        selectedTarget <- self$checkTarget(target,path)
       
        if (selectedTarget != "") {
          return(as.character(selectedTarget))
        }
      }
     
      return("unrecognizable")
    },
     
    checkTarget = function(target,path) {
       
      if (!"character" %in% class(target)) {
        stop("[TargetAssigningFromPathPipe][checkTarget][Error] 
             Checking the type of the variable: target ", class(target))
      }
     
      if (!"character" %in% class(path)) {
        stop("[TargetAssigningFromPathPipe][checkTarget][Error] 
             Checking the type of the variable: path ", class(path))
      }
     
      selectedTarget <- ""
     
      if (stri_detect_fixed(path,target)) {
        selectedTarget <- self$getTargets()[target]
      } 
     
      return(selectedTarget)
    },
     
    getPropertyName = function() {
      return(private$propertyName)
    },
   
    getTargets = function() {
      return(private$targets)
    }
  ),
  
  private = list(
    propertyName = "target",
    targets = list()
  )
)
