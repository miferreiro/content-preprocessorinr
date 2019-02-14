#Class to get the target of the instance
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
      #   targets: (character) Name of the targets property
      #   targetsName: (character) The name of folders
      #   alwaysBeforeDeps: (list) The dependences alwaysBefore (pipes that must 
      #                            be executed before this one)
      #   notAfterDeps: (list) The dependences notAfter (pipes that cannot be 
      #                       executed after this one)
      #Returns:
      #   null
      #     
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
      #
      #Function that preprocesses the instance to obtain the target 
      #
      #Args:
      #   instance: (Instance) instance to preproccess
      #Returns:
      #   The instance with the modifications that have occurred in the pipe
      #         
      if (!"Instance" %in% class(instance)) {
        stop("[TargetAssigningFromPathPipe][pipe][Error] 
                 Checking the type of the variable: instance ", 
                   class(instance))
      }
     
      TypePipe[["private_fields"]][["flowPipes"]] <- 
        list.append(TypePipe[["private_fields"]][["flowPipes"]], "TargetAssigningFromPathPipe")
      
      if (!super$checkCompatibility("TargetAssigningFromPathPipe")) {
        stop("[TargetAssigningFromPathPipe][pipe][Error] Bad compatibility between Pipes.")
      }
      
      instance$getPath() %>>% 
        self$getTarget() %>>%
          {instance$addProperties(.,super$getPropertyName())}

      if (instance$getSpecificProperty("target") %in% "unrecognizable") {
        
        message <- c( "The file: " , instance$getPath() , " has a target unrecognizable")
        
        instance$addProperties(message, "reasonToInvalidate") 
        
        cat("[TargetAssigningFromPathPipe][pipe][Warning] ", message, " \n")
        
        instance$invalidate()
        
        return(instance)
      }
        
      return(instance)

    },
     
    getTarget = function(path) {
      #
      #Function to get the target from a path
      #
      #Args:
      #   path: (character) path to analize
      #Returns:
      #   The target
      #          
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
     
    checkTarget = function(target, path) {
      #
      #Function to check if the target is in the path
      #
      #Args:
      #   path: (character) path to analize
      #   target: (character) target to find in the path
      #Returns:
      #   If the target is found, returns target, else returns ""
      #            
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
      #
      #Getter of targets
      #
      #Args:
      #   null
      #
      #Returns:
      #   value of targets variable
      #
      return(private$targets)
    }
  ),
  
  private = list(
    targets = list()
  )
)
