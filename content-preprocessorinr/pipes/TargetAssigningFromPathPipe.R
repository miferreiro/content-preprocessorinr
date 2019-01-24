#Class to 
#
#
#Variables:
#targets: (list) 
#
TargetAssigningFromPathPipe <- R6Class(
    
  "TargetAssigningFromPathPipe",
  
  inherit = PipeGeneric,
    
  public = list(
        
    initialize = function(targets = list("ham","spam"),
                            targetsName = list("_ham_","_spam_"), 
                              propertyName = "target") {

      if (!"list" %in% class(targets)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
                Checking the type of the variable: targets ", 
                  class(targets))
      }
      
      if (!"list" %in% class(targetsName)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
                Checking the type of the variable: targetsName ", 
                  class(targetsName))
      }
      
      if (!"character" %in% class(propertyName)) {
        stop("[TargetAssigningFromPathPipe][initialize][Error] 
                Checking the type of the variable: propertyName ", 
                  class(propertyName))
      }
      private$targets <- targets
      names(private$targets) <- targetsName
       
      propertyName %>>% 
        super$initialize()
    },    
       
    pipe = function(instance) {
       
      if (!"Instance" %in% class(instance)) {
        stop("[TargetAssigningFromPathPipe][pipe][Error] 
                 Checking the type of the variable: instance ", 
                   class(instance))
      }
     
      instance$getPath() %>>% 
        self$getTarget() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("target") %in% "unrecognizable") {
        message <- c( "The file: " , instance$getPath() , " has a target unrecognizable")
        warning(message)  
        instance$invalidate()
        return(NULL)
      } else {
        return(instance)
      }
    },
     
    getTarget = function(path) {
       
      if (!"character" %in% class(path)) {
        stop("[TargetAssigningFromPathPipe][getTarget][Error] 
                Checking the type of the variable: path ",
                  class(path))
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
                Checking the type of the variable: target ", 
                  class(target))
      }
     
      if (!"character" %in% class(path)) {
        stop("[TargetAssigningFromPathPipe][checkTarget][Error] 
                Checking the type of the variable: path ", 
                  class(path))
      }
     
      selectedTarget <- ""
     
      if (stri_detect_fixed(path,target)) {
        selectedTarget <- self$getTargets()[target]
      } 
     
      return(selectedTarget)
    },
     
    getTargets = function() {
      return(private$targets)
    }
  ),
  
  private = list(
    targets = list()
  )
)
