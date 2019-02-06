deleteInvalidInstances = function(instance) {
  
    if (!"Instance" %in% class(instance)) {
      stop("[deleteInvalidInstances][Error] 
              Checking the type of the variable: instance ", 
                class(instance))
    }
  
    if ( is.null(instance) || !instance$isInstanceValid()) {
      return(FALSE)
    } else {
      return(TRUE)
    }
}
