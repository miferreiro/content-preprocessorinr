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
                              propertyName = "target",  
                                alwaysBeforeDeps = list(), 
                                  notAfterDeps = list()) {

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
      
      private$targets <- targets
      names(private$targets) <- targetsName
       
      super$initialize(propertyName, alwaysBeforeDeps, notAfterDeps)
      
    },    
       
    pipe = function(instance) {
      
      if (!"Instance" %in% class(instance)) {
        stop("[TargetAssigningFromPathPipe][pipe][Error] 
                 Checking the type of the variable: instance ", 
                   class(instance))
      }
     
      TypePipe[["private_fields"]][["flowPipes"]] <- list.append(TypePipe[["private_fields"]][["flowPipes"]], 
                                                                 "TargetAssigningFromPathPipe")
      
      if (!super$checkCompatibility("TargetAssigningFromPathPipe")) {
        stop("[TargetAssigningFromPathPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      # TypePipe[["private_fields"]][["banPipes"]] <- list.append(TypePipe[["private_fields"]][["banPipes"]],
      #                                                           "")
      
      instance$getPath() %>>% 
        self$getTarget() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("target") %in% "unrecognizable") {
        message <- c( "The file: " , instance$getPath() , " has a target unrecognizable")
        instance$addProperties(message, "reasonToInvalidate") 
        warning(message)  
        instance$invalidate()
        return(instance)
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
